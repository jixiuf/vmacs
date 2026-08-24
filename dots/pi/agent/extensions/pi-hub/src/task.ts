// ============================================================================
// Subagent 任务注册表：持久化 tasks.json + 状态机（超时/重试/回收判定）
// 复用 registry.ts 的 tmp+rename 原子写模式
// ============================================================================

import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'

// 惰性读取：测试环境可在 import 后设置 PI_HUB_STATE_DIR 隔离（同 registry.ts 模式）
function stateDir(): string {
  return process.env.PI_HUB_STATE_DIR ?? path.join(os.homedir(), '.pi', 'agent', 'wechat-assistant')
}
function tasksFile(): string {
  return path.join(stateDir(), 'tasks.json')
}

/** 任务默认 TTL：超时后重试，重试达上限标记 timeout */
export const TASK_TTL_MS = 10 * 60_000
/** 任务重试上限（超时后重新分发的次数） */
export const TASK_MAX_ATTEMPTS = 2

export interface SubTask {
  id: string
  /** 人类可读描述 */
  title: string
  /** 子实例名 */
  assignee: string
  status: 'pending' | 'running' | 'done' | 'failed' | 'timeout'
  /** 分发时发给子实例的正文 */
  payload: string
  createdAt: number
  deadline: number
  /** 回传结果原文（[TASK#N结果] 后内容） */
  result?: string
  error?: string
  attempts: number
  /** 完成后不自动回收（调试用） */
  keep?: boolean
}

/** monitor 返回的事件：主实例轮询处理 */
export interface TaskMonitorEvent {
  id: string
  kind: 'retry' | 'timeout' | 'reclaim'
  assignee?: string
}

function readJson<T>(file: string): T | null {
  try {
    return JSON.parse(fs.readFileSync(file, 'utf8')) as T
  } catch {
    return null
  }
}

function writeJson(file: string, data: unknown): void {
  try {
    fs.mkdirSync(path.dirname(file), { recursive: true })
    const tmp = `${file}.tmp`
    fs.writeFileSync(tmp, JSON.stringify(data, null, 2), { mode: 0o600 })
    fs.renameSync(tmp, file)
  } catch {
    // ignore
  }
}

export class TaskRegistry {
  create(opts: { title?: string; assignee: string; payload: string; ttlMs?: number; keep?: boolean }): SubTask {
    const now = Date.now()
    const task: SubTask = {
      id: `TASK-${now}-${Math.random().toString(36).slice(2, 6)}`,
      title: opts.title ?? opts.payload.slice(0, 40),
      assignee: opts.assignee,
      status: 'pending',
      payload: opts.payload,
      createdAt: now,
      deadline: now + (opts.ttlMs ?? TASK_TTL_MS),
      attempts: 0,
      keep: opts.keep,
    }
    const tasks = this.list()
    tasks.push(task)
    writeJson(tasksFile(), tasks)
    return task
  }

  update(id: string, patch: Partial<SubTask>): SubTask | null {
    const tasks = this.list()
    const t = tasks.find((x) => x.id === id)
    if (!t) return null
    Object.assign(t, patch)
    writeJson(tasksFile(), tasks)
    return t
  }

  get(id: string): SubTask | null {
    return this.list().find((x) => x.id === id) ?? null
  }

  list(): SubTask[] {
    return readJson<SubTask[]>(tasksFile()) ?? []
  }

  /** 某实例是否所有任务均已结束（可回收） */
  instanceDone(assignee: string): boolean {
    const tasks = this.list().filter((t) => t.assignee === assignee)
    if (tasks.length === 0) return false
    return tasks.every((t) => t.status === 'done' || t.status === 'failed' || t.status === 'timeout')
  }

  /**
   * 状态机扫描：返回需要主实例处理的事件。
   * - 超时未达上限 → 标记 pending（重试）并返回 retry 事件（主实例重新分发）
   * - 超时达上限 → 标记 timeout 并返回 timeout 事件
   * - 某实例全部任务结束（且无 keep）→ 返回 reclaim 事件（主实例发 /quit）
   */
  monitor(now = Date.now()): TaskMonitorEvent[] {
    const events: TaskMonitorEvent[] = []
    const tasks = this.list()
    for (const t of tasks) {
      if ((t.status === 'pending' || t.status === 'running') && now > t.deadline) {
        if (t.attempts < TASK_MAX_ATTEMPTS) {
          this.update(t.id, {
            status: 'pending',
            attempts: t.attempts + 1,
            deadline: now + TASK_TTL_MS,
            error: `超时，第 ${t.attempts + 1} 次重试`,
          })
          events.push({ id: t.id, kind: 'retry', assignee: t.assignee })
        } else {
          this.update(t.id, { status: 'timeout', error: '超时且重试达上限' })
          events.push({ id: t.id, kind: 'timeout', assignee: t.assignee })
        }
      }
    }
    // 回收判定（用最新任务列表）
    const reclaimSet = new Set<string>()
    for (const t of this.list()) {
      if (!t.keep && t.status === 'done') reclaimSet.add(t.assignee)
    }
    for (const a of reclaimSet) {
      if (this.instanceDone(a)) events.push({ id: '', kind: 'reclaim', assignee: a })
    }
    return events
  }
}
