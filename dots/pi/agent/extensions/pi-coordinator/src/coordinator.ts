// ============================================================================
// pi-coordinator 核心：实例注册表、全局锁、接管队列、指令/消息通道
// 独立于 pi-wechat-assistant，任何扩展可通过 globalThis.__PI_COORDINATOR__ 复用
// ============================================================================

import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import * as http from 'node:http'
import { execFile } from 'node:child_process'

// --- 状态目录（与 wechat-assistant 共用，保证通道互通） ---

const STATE_DIR = path.join(os.homedir(), '.pi', 'agent', 'wechat-assistant')
const INSTANCES_FILE = path.join(STATE_DIR, 'instances.json')
const TAKEOVER_FILE = path.join(STATE_DIR, 'takeover.json')
const LOCK_FILE = path.join(STATE_DIR, 'session.lock')
const GLOBAL_LOCK_FILE = path.join(STATE_DIR, 'coordinator-lock.json')
const MESSAGE_FILE = path.join(STATE_DIR, 'coordinator-messages.json')

/** 接管请求超时：60 秒未处理视为过期 */
const TAKEOVER_TTL_MS = 60_000
/** 全局锁心跳超时：超过此时长未续约视为持有者下线 */
const GLOBAL_LOCK_TTL_MS = 10_000
/** 消息队列 TTL：5 分钟 */
const MESSAGE_TTL_MS = 5 * 60_000

// --- 类型 ---

export interface InstanceInfo {
  name: string
  pid: number
  cwd: string
  sessionId: string
  lastSeen: number
  host?: string
}

export interface TakeoverRequest {
  targetName: string
  targetPid: number
  fromName: string
  /** 业务标识（如 wechat 表示微信接管；空为通用接管） */
  capability?: string
  payload?: unknown
  timestamp: number
}

export interface CoordinatorMessage {
  id: string
  from: string
  to: string
  text: string
  timestamp: number
}

export interface GlobalLock {
  name: string
  pid: number
  capability?: string
  lastSeen: number
}

export interface RemoteHostConfig {
  target: string
  port?: number
}

// --- 文件辅助 ---

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

function isProcessRunning(pid: number): boolean {
  try {
    process.kill(pid, 0)
    return true
  } catch {
    return false
  }
}

// --- 实例注册表 ---

export function registerInstance(info: Omit<InstanceInfo, 'lastSeen'>): void {
  const instances = readJson<Record<string, InstanceInfo>>(INSTANCES_FILE) ?? {}
  instances[info.name] = { ...info, host: info.host ?? os.hostname(), lastSeen: Date.now() }
  writeJson(INSTANCES_FILE, instances)
}

export function unregisterInstance(name: string, pid: number): void {
  const instances = readJson<Record<string, InstanceInfo>>(INSTANCES_FILE) ?? {}
  const entry = instances[name]
  if (entry && entry.pid === pid) {
    delete instances[name]
    writeJson(INSTANCES_FILE, instances)
  }
}

export function listInstances(): InstanceInfo[] {
  const instances = readJson<Record<string, InstanceInfo>>(INSTANCES_FILE) ?? {}
  const alive: InstanceInfo[] = []
  let changed = false
  for (const name of Object.keys(instances).sort()) {
    const info = instances[name]
    if (isProcessRunning(info.pid)) {
      alive.push(info)
    } else {
      delete instances[name]
      changed = true
    }
  }
  if (changed) writeJson(INSTANCES_FILE, instances)
  return alive
}

// --- 接管队列（本地文件，本机实例轮询） ---

export function writeLocalTakeoverRequest(req: TakeoverRequest): void {
  writeJson(TAKEOVER_FILE, req)
}

export function readTakeoverRequest(): TakeoverRequest | null {
  const req = readJson<TakeoverRequest>(TAKEOVER_FILE)
  if (!req) return null
  if (Date.now() - req.timestamp > TAKEOVER_TTL_MS) {
    clearTakeoverRequest()
    return null
  }
  return req
}

export function clearTakeoverRequest(): void {
  try {
    fs.unlinkSync(TAKEOVER_FILE)
  } catch {
    // ignore
  }
}

// --- 全局锁（跨机器仲裁：谁在提供服务/轮询） ---

function readGlobalLockFile(): GlobalLock | null {
  const d = readJson<GlobalLock>(GLOBAL_LOCK_FILE)
  return d && d.name ? d : null
}

export function coordinatorTryLock(name: string, pid: number, capability?: string, force = false): boolean {
  const now = Date.now()
  const cur = readGlobalLockFile()
  if (cur) {
    if (cur.name !== name || cur.capability !== capability) {
      if (force || now - cur.lastSeen > GLOBAL_LOCK_TTL_MS) {
        writeJson(GLOBAL_LOCK_FILE, { name, pid, capability, lastSeen: now })
        return true
      }
      return false
    }
    writeJson(GLOBAL_LOCK_FILE, { name, pid, capability, lastSeen: now })
    return true
  }
  writeJson(GLOBAL_LOCK_FILE, { name, pid, capability, lastSeen: now })
  return true
}

export function coordinatorReleaseLock(name: string, capability?: string): void {
  const cur = readGlobalLockFile()
  if (cur && cur.name === name && (!capability || cur.capability === capability)) {
    writeJson(GLOBAL_LOCK_FILE, {})
  }
}

export function getGlobalLockHolder(): { name: string; capability?: string } | null {
  const cur = readGlobalLockFile()
  if (!cur) return null
  if (Date.now() - cur.lastSeen > GLOBAL_LOCK_TTL_MS) {
    writeJson(GLOBAL_LOCK_FILE, {})
    return null
  }
  return { name: cur.name, capability: cur.capability }
}

// --- 协调中心 HTTP 服务（服务器侧） ---

/** 远程接管队列：targetName → 请求（等局域网机器轮询拉取） */
const remoteQueue = new Map<string, TakeoverRequest>()

/** 活跃客户端实例（通过协调中心轮询的客户端）：name → lastSeen（毫秒） */
const activeClients = new Map<string, number>()
const ACTIVE_CLIENT_TTL_MS = 60_000

/** 客户端轮询 /takeover 时登记为活跃（用于实例列表可见性） */
function markActiveClient(name: string): void {
  if (!name) return
  activeClients.set(name, Date.now())
  const now = Date.now()
  for (const [k, ts] of activeClients) {
    if (now - ts > ACTIVE_CLIENT_TTL_MS) activeClients.delete(k)
  }
}

export function enqueueRemoteTakeover(req: TakeoverRequest): void {
  remoteQueue.set(req.targetName, req)
  // 无条件预占全局锁：让当前持有者心跳失败自动让位，目标实例可续约接管
  if (req.capability) {
    writeJson(GLOBAL_LOCK_FILE, {
      name: req.targetName,
      pid: req.targetPid,
      capability: req.capability,
      lastSeen: Date.now(),
    })
  }
}

export function startCoordinatorServer(
  port: number,
  local: { name: string; pid: number; cwd: string; host?: string },
  remoteNames: string[],
): http.Server {
  const server = http.createServer(async (req, res) => {
    try {
      const url = new URL(req.url ?? '/', 'http://localhost')

      // 局域网机器轮询：有给它的接管请求吗（顺带登记活跃客户端）
      if (req.method === 'GET' && url.pathname === '/takeover') {
        const name = url.searchParams.get('name') ?? ''
        markActiveClient(name)
        const pid = Number(url.searchParams.get('pid') ?? '0')
        const pending = remoteQueue.get(name)
        if (pending && (pending.targetPid === 0 || pid === 0 || pending.targetPid === pid)) {
          remoteQueue.delete(name)
          res.writeHead(200, { 'Content-Type': 'application/json' })
          res.end(JSON.stringify({ ok: true, req: pending }))
        } else {
          res.writeHead(200, { 'Content-Type': 'application/json' })
          res.end(JSON.stringify({ ok: false }))
        }
        return
      }

      // 局域网机器切到服务器实例 / 发送指令到服务器实例：登记接管或命令
      if (req.method === 'POST' && url.pathname === '/takeover') {
        let body = ''
        for await (const chunk of req) body += chunk
        const reqData = JSON.parse(body) as TakeoverRequest
        // 切换请求到达：无条件把锁预占给目标实例（当前持有者心跳失败自动让位，目标实例可续约）
        if (reqData.capability) {
          writeJson(GLOBAL_LOCK_FILE, {
            name: reqData.targetName,
            pid: reqData.targetPid,
            capability: reqData.capability,
            lastSeen: Date.now(),
          })
        }
        if (reqData.targetPid === process.pid) {
          writeLocalTakeoverRequest(reqData)
        } else {
          remoteQueue.set(reqData.targetName, reqData)
        }
        res.writeHead(200, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ ok: true }))
        return
      }

      // 全局锁：
      //  GET /lock?name=X&pid=P[&capability=C][&force=1] → 获取/续约（客户端协议）
      //  GET /lock → 仅查询持有者
      //  POST /lock {name,pid,capability,force} → 获取/续约
      if (url.pathname === '/lock') {
        if (req.method === 'GET') {
          const name = url.searchParams.get('name')
          if (name) {
            const pid = Number(url.searchParams.get('pid') ?? '0')
            const capability = url.searchParams.get('capability') ?? undefined
            const force = url.searchParams.get('force') === '1'
            const ok = coordinatorTryLock(name, pid, capability, force)
            res.writeHead(200, { 'Content-Type': 'application/json' })
            res.end(JSON.stringify({ ok, holder: getGlobalLockHolder() }))
          } else {
            res.writeHead(200, { 'Content-Type': 'application/json' })
            res.end(JSON.stringify({ holder: getGlobalLockHolder() }))
          }
          return
        }
        if (req.method === 'POST') {
          let body = ''
          for await (const chunk of req) body += chunk
          const data = JSON.parse(body) as { name: string; pid: number; capability?: string; force?: boolean }
          const ok = coordinatorTryLock(data.name, data.pid, data.capability, !!data.force)
          res.writeHead(200, { 'Content-Type': 'application/json' })
          res.end(JSON.stringify({ ok, holder: getGlobalLockHolder() }))
          return
        }
      }

      // 释放全局锁
      if (req.method === 'POST' && url.pathname === '/unlock') {
        let body = ''
        for await (const chunk of req) body += chunk
        const data = JSON.parse(body) as { name: string; capability?: string }
        coordinatorReleaseLock(data.name, data.capability)
        res.writeHead(200, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ ok: true }))
        return
      }

      // 指令/消息投递：POST /command 或 /message
      if (req.method === 'POST' && (url.pathname === '/command' || url.pathname === '/message')) {
        let body = ''
        for await (const chunk of req) body += chunk
        const isCommand = url.pathname === '/command'
        const data = JSON.parse(body) as { to?: string; text?: string; from?: string }
        // 转发到本地接管队列或消息文件，由目标实例定时器拉取
        if (isCommand && data.to === local.name) {
          writeLocalTakeoverRequest({
            targetName: local.name,
            targetPid: process.pid,
            fromName: data.from ?? 'remote',
            capability: 'command',
            payload: { command: data.text },
            timestamp: Date.now(),
          })
        } else {
          const messages = readJson<CoordinatorMessage[]>(MESSAGE_FILE) ?? []
          messages.push({
            id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
            from: data.from ?? 'remote',
            to: data.to ?? '',
            text: data.text ?? '',
            timestamp: Date.now(),
          })
          writeJson(MESSAGE_FILE, messages.slice(-100))
        }
        res.writeHead(200, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ ok: true }))
        return
      }

      // 消息拉取：投递后立即删除，避免拉取端重复消费同一条消息
      if (req.method === 'GET' && url.pathname === '/messages') {
        const to = url.searchParams.get('to') ?? ''
        const all = readJson<CoordinatorMessage[]>(MESSAGE_FILE) ?? []
        const delivered = all.filter((m) => m.to === to)
        const remaining = all.filter((m) => m.to !== to)
        writeJson(MESSAGE_FILE, remaining)
        res.writeHead(200, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ ok: true, messages: delivered }))
        return
      }

      // 服务器本地实例信息
      if (req.method === 'GET' && url.pathname === '/instances') {
        res.writeHead(200, { 'Content-Type': 'application/json' })
        const clients = [...activeClients.keys()]
          .filter((n) => n !== local.name)
          .map((n) => ({ name: n, pid: 0, cwd: '', host: undefined }))
        res.end(
          JSON.stringify({
            local: [{ name: local.name, pid: local.pid, cwd: local.cwd, host: local.host }],
            remoteNames,
            clients,
          }),
        )
        return
      }

      res.writeHead(404)
      res.end('not found')
    } catch {
      res.writeHead(500)
      res.end('error')
    }
  })
  server.listen(port)
  return server
}

// --- 客户端（局域网机器） ---

export async function pollCoordinator(baseUrl: string, name: string, pid: number): Promise<TakeoverRequest | null> {
  try {
    const res = await fetch(`${baseUrl}/takeover?name=${encodeURIComponent(name)}&pid=${pid}`)
    if (!res.ok) return null
    const data = (await res.json()) as { ok: boolean; req?: TakeoverRequest }
    return data.ok && data.req ? data.req : null
  } catch {
    return null
  }
}

export async function notifyCoordinator(baseUrl: string, req: TakeoverRequest): Promise<void> {
  const res = await fetch(`${baseUrl}/takeover`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(req),
  })
  if (!res.ok) throw new Error(`coordinator responded ${res.status}`)
}

export async function fetchCoordinatorInstances(
  baseUrl: string,
): Promise<{
  local: { name: string; pid: number; cwd: string; host?: string }[]
  remoteNames: string[]
  clients?: { name: string; pid: number; cwd?: string; host?: string }[]
} | null> {
  try {
    const res = await fetch(`${baseUrl}/instances`)
    if (!res.ok) return null
    return (await res.json()) as {
      local: { name: string; pid: number; cwd: string; host?: string }[]
      remoteNames: string[]
      clients?: { name: string; pid: number; cwd?: string; host?: string }[]
    }
  } catch {
    return null
  }
}

export async function sendCommandToRemote(baseUrl: string, to: string, command: string, from?: string): Promise<void> {
  const res = await fetch(`${baseUrl}/command`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ to, text: command, from }),
  })
  if (!res.ok) throw new Error(`coordinator responded ${res.status}`)
}

export async function sendMessageToRemote(baseUrl: string, to: string, text: string, from?: string): Promise<void> {
  const res = await fetch(`${baseUrl}/message`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ to, text, from }),
  })
  if (!res.ok) throw new Error(`coordinator responded ${res.status}`)
}

export async function fetchMessages(baseUrl: string, to: string): Promise<CoordinatorMessage[]> {
  try {
    const res = await fetch(`${baseUrl}/messages?to=${encodeURIComponent(to)}`)
    if (!res.ok) return []
    const data = (await res.json()) as { ok: boolean; messages: CoordinatorMessage[] }
    return data.messages ?? []
  } catch {
    return []
  }
}

/** 本地消息：发给本实例（服务器侧直接写文件） */
export function writeLocalMessages(messages: CoordinatorMessage[]): void {
  writeJson(MESSAGE_FILE, messages.slice(-100))
}

export function readLocalMessages(to?: string): CoordinatorMessage[] {
  const messages = readJson<CoordinatorMessage[]>(MESSAGE_FILE) ?? []
  const now = Date.now()
  const fresh = messages.filter((m) => now - m.timestamp < MESSAGE_TTL_MS)
  if (fresh.length !== messages.length) writeJson(MESSAGE_FILE, fresh)
  return to ? fresh.filter((m) => m.to === to) : fresh
}

/** 消费发给指定实例的消息：读取并立即删除，避免重复投递 */
export function consumeLocalMessages(to?: string): CoordinatorMessage[] {
  const messages = readLocalMessages()
  const delivered = to ? messages.filter((m) => m.to === to) : messages
  const remaining = to ? messages.filter((m) => m.to !== to) : []
  writeJson(MESSAGE_FILE, remaining)
  return delivered
}

// --- SSH 通道（机器间可互访时用） ---

function sshExec(target: string, port: number | undefined, command: string, timeoutMs = 15000): Promise<string> {
  return new Promise((resolve, reject) => {
    const args = ['-o', 'BatchMode=yes', '-o', 'ConnectTimeout=5']
    if (port && port !== 22) args.push('-p', String(port))
    args.push(target, command)
    execFile('ssh', args, { timeout: timeoutMs, maxBuffer: 64 * 1024 }, (err, stdout, stderr) => {
      if (err) {
        reject(new Error(stderr.trim() || err.message))
        return
      }
      resolve(stdout)
    })
  })
}

export async function listRemoteInstances(cfg: Record<string, RemoteHostConfig>): Promise<InstanceInfo[]> {
  const result: InstanceInfo[] = []
  for (const [name, remote] of Object.entries(cfg)) {
    try {
      const stdout = await sshExec(remote.target, remote.port, `cat ${STATE_DIR}/instances.json 2>/dev/null || echo '{}'`)
      const data = JSON.parse(stdout || '{}') as Record<string, InstanceInfo>
      const entry = data[name]
      if (entry && entry.name) result.push(entry)
    } catch {
      // 远程不可达时跳过
    }
  }
  return result
}

export async function writeRemoteTakeoverRequest(remote: RemoteHostConfig, req: TakeoverRequest): Promise<void> {
  const payload = JSON.stringify(req).replace(/'/g, `'\\''`)
  const command = `mkdir -p ${STATE_DIR} && echo '${payload}' > ${STATE_DIR}/takeover.json`
  await sshExec(remote.target, remote.port, command)
}
