// ============================================================================
// 传输层：协调中心 HTTP 服务 + 客户端拉取（保留现有 8089 模式，行为兼容）
// 统一 Envelope 协议，取代旧的 takeover/messages/lock 三套端点
// ============================================================================

import * as http from 'node:http'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { execFile } from 'node:child_process'
import type { Envelope, InstanceInfo, TakeoverRequest } from './types.js'
import { EnvelopeQueue } from './queue.js'
import { preassignLock, coordinatorTryLock, coordinatorReleaseLock, getGlobalLockHolder } from './lock.js'

// --- 状态目录 ---

const STATE_DIR =
  process.env.PI_HUB_STATE_DIR ?? path.join(os.homedir(), '.pi', 'agent', 'wechat-assistant')
const LAST_MSG_FILE = path.join(STATE_DIR, 'last-wechat-msg.json')

/** 最后一条微信对话（跨实例共享，协调中心为权威存储） */
export interface LastWechatMsg {
  userId: string
  userMsg: string
  aiMsg: string
  ts: number
}

export function readLastMsgLocal(): LastWechatMsg | null {
  try {
    const d = JSON.parse(fs.readFileSync(LAST_MSG_FILE, 'utf8')) as LastWechatMsg
    return d && (d.userMsg || d.aiMsg) ? d : null
  } catch {
    return null
  }
}

export function writeLastMsgLocal(data: LastWechatMsg): void {
  try {
    fs.writeFileSync(LAST_MSG_FILE, JSON.stringify(data), { mode: 0o600 })
  } catch {
    // ignore
  }
}

/** 活跃客户端实例（通过协调中心轮询的客户端）：name → lastSeen（毫秒） */
const activeClients = new Map<string, number>()
/** 客户端实例名 → 主机名（从 /inbox 轮询登记，用于主机名/实例名互查） */
const clientHosts = new Map<string, string>()
const ACTIVE_CLIENT_TTL_MS = 60_000

function markActiveClient(name: string, host?: string): void {
  if (!name) return
  activeClients.set(name, Date.now())
  if (host) clientHosts.set(name, host)
  const now = Date.now()
  for (const [k, ts] of activeClients) {
    if (now - ts > ACTIVE_CLIENT_TTL_MS) {
      activeClients.delete(k)
      clientHosts.delete(k)
    }
  }
}

export function listActiveClients(): string[] {
  return [...activeClients.keys()]
}

/** 活跃客户端的实例名 → 主机名（用于实例列表显示 host） */
export function clientHostName(name: string): string | undefined {
  return clientHosts.get(name)
}

/** 远程接管队列：targetName → 请求（等局域网机器轮询拉取） */
const remoteQueue = new Map<string, TakeoverRequest>()

/** 服务器 → 局域网：入队远程接管请求（目标客户端下次 /inbox 轮询会取到） */
export function enqueueRemoteTakeover(req: TakeoverRequest): void {
  remoteQueue.set(req.targetName, req)
  if (req.capability) {
    preassignLock(req.targetName, req.targetPid, req.capability)
  }
}

// ============================================================================
// 协调中心（服务器侧）
// ============================================================================

export function startCoordinatorServer(
  port: number,
  local: { name: string; pid: number; cwd: string; host?: string },
  remoteNames: string[],
  queue: EnvelopeQueue,
): http.Server {
  const server = http.createServer(async (req, res) => {
    try {
      const url = new URL(req.url ?? '/', 'http://localhost')

      // 客户端轮询：取出发给自己的 envelope（ack 语义在客户端处理）
      if (req.method === 'GET' && url.pathname === '/inbox') {
        const name = url.searchParams.get('name') ?? ''
        const host = url.searchParams.get('host') ?? ''
        markActiveClient(name, host)
        const pid = Number(url.searchParams.get('pid') ?? '0')
        const envelopes = queue.dequeue(name).map(({ env }) => env)
        // 服务器 → 局域网：把远程接管队列中的请求作为 takeover envelope 返回
        // 匹配：实例名或主机名任一命中（兼容 remoteInstanceNames 配 hostname 的旧配置）
        const byName = remoteQueue.get(name)
        const byHost = host ? [...remoteQueue.entries()].find(([k, r]) => r.targetName === host || clientHosts.get(r.targetName) === host) : undefined
        const entry = byName ? [name, byName] as const : byHost
        if (entry) {
          const [qKey, pending] = entry
          if (pending && (pending.targetPid === 0 || pid === 0 || pending.targetPid === pid)) {
            remoteQueue.delete(qKey)
            envelopes.push({
              type: 'takeover',
              id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
              from: pending.fromName,
              to: pending.targetName,
              capability: pending.capability ?? '',
              ts: pending.timestamp,
            })
          }
        }
        res.writeHead(200, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ ok: true, envelopes }))
        return
      }

      // 客户端入队：message / command / takeover / broadcast / lock
      if (req.method === 'POST' && url.pathname === '/envelope') {
        let body = ''
        for await (const chunk of req) body += chunk
        const env = JSON.parse(body) as Envelope
        // 接管请求到达：无条件预占锁（当前持有者心跳失败自动让位）
        if (env.type === 'takeover') {
          preassignLock(env.to, 0, env.capability)
        }
        queue.enqueue(env)
        res.writeHead(200, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ ok: true }))
        return
      }

      // 全局锁（服务器权威）：客户端通过 HTTP 请求锁，服务器端是唯一仲裁者
      //  GET /lock → 仅查询持有者
      //  GET /lock?name=X&pid=P&capability=C[&force=1] → 获取/续约
      //  POST /unlock {name, capability} → 释放
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
          const data = JSON.parse(body) as { name?: string; pid?: number; capability?: string; force?: boolean }
          const ok = coordinatorTryLock(data.name ?? '', data.pid ?? 0, data.capability, !!data.force)
          res.writeHead(200, { 'Content-Type': 'application/json' })
          res.end(JSON.stringify({ ok, holder: getGlobalLockHolder() }))
          return
        }
      }

      if (req.method === 'POST' && url.pathname === '/unlock') {
        let body = ''
        for await (const chunk of req) body += chunk
        const data = JSON.parse(body) as { name?: string; capability?: string }
        coordinatorReleaseLock(data.name ?? '', data.capability)
        res.writeHead(200, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ ok: true }))
        return
      }

      // 最后一条微信对话（协调中心权威存储，跨实例共享）
      //  GET /lastmsg → 读取
      //  POST /lastmsg {userId, userMsg, aiMsg} → 写入
      if (url.pathname === '/lastmsg') {
        if (req.method === 'GET') {
          const d = readLastMsgLocal()
          res.writeHead(200, { 'Content-Type': 'application/json' })
          res.end(JSON.stringify({ ok: true, msg: d }))
          return
        }
        if (req.method === 'POST') {
          let body = ''
          for await (const chunk of req) body += chunk
          const data = JSON.parse(body) as Partial<LastWechatMsg>
          const prev = readLastMsgLocal()
          writeLastMsgLocal({
            userId: data.userId ?? prev?.userId ?? '',
            userMsg: (data.userMsg ?? prev?.userMsg ?? '').slice(0, 200),
            aiMsg: (data.aiMsg ?? prev?.aiMsg ?? '').slice(0, 500),
            ts: Date.now(),
          })
          res.writeHead(200, { 'Content-Type': 'application/json' })
          res.end(JSON.stringify({ ok: true }))
          return
        }
      }

      // 实例列表（服务器本地 + 活跃客户端）
      if (req.method === 'GET' && url.pathname === '/instances') {
        const clients = [...activeClients.keys()]
          .filter((n) => n !== local.name)
          .map((n) => ({ name: n, pid: 0, cwd: '', host: undefined }))
        res.writeHead(200, { 'Content-Type': 'application/json' })
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

// ============================================================================
// 客户端（局域网机器）
// ============================================================================

export async function fetchInbox(baseUrl: string, name: string, host?: string): Promise<Envelope[]> {
  try {
    const res = await fetch(`${baseUrl}/inbox?name=${encodeURIComponent(name)}${host ? `&host=${encodeURIComponent(host)}` : ''}`)
    if (!res.ok) return []
    const data = (await res.json()) as { ok: boolean; envelopes: Envelope[] }
    return data.envelopes ?? []
  } catch {
    return []
  }
}

/** 客户端向协调中心读取最后一条微信对话 */
export async function fetchLastMsgRemote(baseUrl: string): Promise<LastWechatMsg | null> {
  try {
    const res = await fetch(`${baseUrl}/lastmsg`)
    if (!res.ok) return null
    const data = (await res.json()) as { ok: boolean; msg?: LastWechatMsg | null }
    return data.msg ?? null
  } catch {
    return null
  }
}

/** 客户端向协调中心写入最后一条微信对话 */
export async function pushLastMsgRemote(baseUrl: string, data: Partial<LastWechatMsg>): Promise<void> {
  try {
    await fetch(`${baseUrl}/lastmsg`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(data),
    })
  } catch {
    // ignore
  }
}

/** 客户端向协调中心请求/续约全局锁（服务器端是唯一仲裁者）
 * 返回 unreachable=true 表示协调中心不可达（服务器 pi 可能已停），
 * 调用方应降级为本地接管而不是让位。 */
export async function requestRemoteLock(
  baseUrl: string,
  name: string,
  pid: number,
  capability?: string,
  force = false,
): Promise<{ ok: boolean; unreachable?: boolean; holder?: { name?: string } | null }> {
  try {
    const query = `name=${encodeURIComponent(name)}&pid=${pid}${capability ? `&capability=${encodeURIComponent(capability)}` : ''}${force ? '&force=1' : ''}`
    const res = await fetch(`${baseUrl}/lock?${query}`)
    if (!res.ok) return { ok: false }
    return (await res.json()) as { ok: boolean; holder?: { name?: string } | null }
  } catch {
    return { ok: false, unreachable: true }
  }
}

/** 客户端释放协调中心全局锁 */
export async function releaseRemoteLock(
  baseUrl: string,
  name: string,
  capability?: string,
): Promise<void> {
  try {
    await fetch(`${baseUrl}/unlock`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ name, capability }),
    })
  } catch {
    // ignore
  }
}

export async function postEnvelope(baseUrl: string, env: Envelope): Promise<void> {
  const res = await fetch(`${baseUrl}/envelope`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(env),
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

// ============================================================================
// SSH 通道（机器间可互访时用，保留向后兼容）
// ============================================================================

export interface RemoteHostConfig {
  target: string
  port?: number
}

export async function listRemoteInstances(
  cfg: Record<string, RemoteHostConfig>,
  instancesFile: string,
): Promise<InstanceInfo[]> {
  const result: InstanceInfo[] = []
  for (const [name, remote] of Object.entries(cfg)) {
    try {
      const stdout = await sshExec(remote.target, remote.port, `cat ${instancesFile} 2>/dev/null || echo '{}'`)
      const data = JSON.parse(stdout || '{}') as Record<string, InstanceInfo>
      const entry = data[name]
      if (entry && entry.name) result.push(entry)
    } catch {
      // 远程不可达时跳过
    }
  }
  return result
}

export async function writeRemoteTakeoverRequest(
  remote: RemoteHostConfig,
  req: TakeoverRequest,
  takeoverFile: string,
): Promise<void> {
  const payload = JSON.stringify(req).replace(/'/g, `'\\''`)
  const command = `mkdir -p $(dirname ${takeoverFile}) && echo '${payload}' > ${takeoverFile}`
  await sshExec(remote.target, remote.port, command)
}

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
