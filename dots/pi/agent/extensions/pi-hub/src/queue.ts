// ============================================================================
// 可靠消息队列：JSON 文件存储 + ack 语义 + 幂等去重 + TTL
// 取代旧的"读即删"机制（consumeLocalMessages / GET /messages 先删后返 → 处理失败即丢）
//
// 仅处理点对点类型（message / command / takeover）；
// lock 走 lock.ts 文件，broadcast 走独立广播文件（多消费者语义不同，不入队）。
//
// 语义：
//   enqueue(env)                        → 追加到文件
//   dequeue(to) → [{env, ack()}]        → 取出发给 to 的待处理消息；ack() 后从文件删除
//   unacked 超时（TTL）后自动重投       → 处理失败不丢
//   已处理 id 集合（内存 + 落盘）       → 重投不重复进会话（幂等）
// ============================================================================

import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import type { Envelope } from './types.js'
import { makeEnvelopeId } from './types.js'

// 惰性读取：测试环境可在 import 后设置 PI_HUB_STATE_DIR 隔离
function stateDir(): string {
  return process.env.PI_HUB_STATE_DIR ?? path.join(os.homedir(), '.pi', 'agent', 'wechat-assistant')
}
function queueFile(): string {
  return path.join(stateDir(), 'coordinator-queue.json')
}
const DEDUP_FILE = (): string => path.join(stateDir(), 'coordinator-processed.json')

/** 消息在队列中的最大存活时间：超过则视为投递失败，重投。
 * 2min 内未 ack（客户端离线/处理失败）即重投，attempts 上限后丢弃；
 * 缩短滞留窗口，避免死消息（to 永不匹配）长期占用文件。 */
const ENVELOPE_TTL_MS = 2 * 60_000
/** 已处理 id 保留时间：超过则从去重集清理 */
const DEDUP_TTL_MS = 30 * 60_000

interface QueueEntry {
  env: Envelope
  /** 首次入队时间；ack 前超时 → 重投（重新戳时间） */
  enqueuedAt: number
  /** 重投次数（上限后丢弃，防死循环） */
  attempts: number
}

const MAX_ATTEMPTS = 3

function readJson<T>(file: string): T | null {
  try {
    return JSON.parse(fs.readFileSync(file, 'utf8')) as T
  } catch {
    return null
  }
}

function writeJson(file: string, data: unknown): void {
  try {
    fs.writeFileSync(file, JSON.stringify(data, null, 2), { mode: 0o600 })
  } catch {
    // ignore
  }
}

// --- 已处理去重集（内存 + 落盘） ---

class DedupSet {
  private readonly map = new Map<string, number>() // id → 处理时间

  constructor() {
    const data = readJson<Array<{ id: string; ts: number }>>(DEDUP_FILE())
    const now = Date.now()
    for (const { id, ts } of data ?? []) {
      if (now - ts < DEDUP_TTL_MS) this.map.set(id, ts)
    }
    this.persist()
  }

  contains(id: string): boolean {
    return this.map.has(id)
  }

  add(id: string): void {
    this.map.set(id, Date.now())
    this.persist()
  }

  private persist(): void {
    const data = [...this.map.entries()].map(([id, ts]) => ({ id, ts }))
    writeJson(DEDUP_FILE(), data)
  }
}

// ============================================================================

export class EnvelopeQueue {
  private readonly dedup = new DedupSet()

  // --- 入队 ---

  enqueue(env: Omit<Envelope, 'id'> & { id?: string }): void {
    const full: Envelope = { ...env, id: env.id ?? makeEnvelopeId() } as Envelope
    const now = Date.now()
    const entries = this.readEntries()
    // 顺带清理过期/超限条目（enqueue 方即队列所有者，无跨实例风险；
    // 避免 to 永不匹配的死消息（@host/旧 pid）无限积压）
    const alive = entries.filter(
      (e) => now - e.enqueuedAt <= ENVELOPE_TTL_MS && e.attempts < MAX_ATTEMPTS,
    )
    alive.push({ env: full, enqueuedAt: now, attempts: 0 })
    this.writeEntries(alive.slice(-500))
  }

  // --- 取出（处理成功才 ack） ---

  dequeue(to: string): Array<{ env: Envelope; ack: () => void }> {
    const entries = this.readEntries()
    const now = Date.now()
    const mine = entries.filter((e) => this.isFor(e.env, to))
    const rest = entries.filter((e) => !this.isFor(e.env, to))

    // 超时未 ack → 重投（重置计时 + 计数）；超过上限丢弃
    const pending: Array<{ env: Envelope; ack: () => void }> = []
    const remaining: QueueEntry[] = []

    for (const e of mine) {
      const expired = now - e.enqueuedAt > ENVELOPE_TTL_MS
      if (e.attempts >= MAX_ATTEMPTS) {
        // 重投超限：丢弃（避免死循环），记录日志由调用方感知
        continue
      }
      if (expired) {
        // 重投：重置计时
        remaining.push({ env: e.env, enqueuedAt: now, attempts: e.attempts + 1 })
        continue
      }
      // 幂等：已处理过的不再取出
      if (this.dedup.contains(e.env.id)) continue
      pending.push({
        env: e.env,
        ack: () => this.ack(e.env.id),
      })
    }

    this.writeEntries([...rest, ...remaining])
    return pending
  }

  private ack(id: string): void {
    const entries = this.readEntries()
    const rest = entries.filter((e) => e.env.id !== id)
    this.writeEntries(rest)
    this.dedup.add(id)
  }

  private isFor(env: Envelope, to: string): boolean {
    switch (env.type) {
      case 'message':
      case 'command':
      case 'takeover':
        return env.to === to
      default:
        return false
    }
  }

  private readEntries(): QueueEntry[] {
    return readJson<QueueEntry[]>(queueFile()) ?? []
  }

  private writeEntries(entries: QueueEntry[]): void {
    writeJson(queueFile(), entries)
  }

  /** 队列长度（调试用） */
  get pendingCount(): number {
    return this.readEntries().length
  }
}
