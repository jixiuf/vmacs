// ============================================================================
// 传输层：协调中心 HTTP 服务 + 客户端拉取（保留现有 8089 模式，行为兼容）
// 统一 Envelope 协议，取代旧的 takeover/messages/lock 三套端点
// ============================================================================

import * as http from 'node:http'
import { execFile } from 'node:child_process'
import type { Envelope, InstanceInfo, TakeoverRequest } from './types.js'
import { EnvelopeQueue } from './queue.js'
import { preassignLock, coordinatorTryLock, coordinatorReleaseLock, getGlobalLockHolder } from './lock.js'

// --- 状态目录 ---

const STATE_DIR = '/tmp' // 占位，实际由调用方传入

/** 活跃客户端实例（通过协调中心轮询的客户端）：name → lastSeen（毫秒） */
const activeClients = new Map<string, number>()
const ACTIVE_CLIENT_TTL_MS = 60_000

function markActiveClient(name: string): void {
  if (!name) return
  activeClients.set(name, Date.now())
  const now = Date.now()
  for (const [k, ts] of activeClients) {
    if (now - ts > ACTIVE_CLIENT_TTL_MS) activeClients.delete(k)
  }
}

export function listActiveClients(): string[] {
  return [...activeClients.keys()]
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
        markActiveClient(name)
        const pid = Number(url.searchParams.get('pid') ?? '0')
        const envelopes = queue.dequeue(name).map(({ env }) => env)
        // 服务器 → 局域网：把远程接管队列中的请求作为 takeover envelope 返回
        const pending = remoteQueue.get(name)
        if (pending && (pending.targetPid === 0 || pid === 0 || pending.targetPid === pid)) {
          remoteQueue.delete(name)
          envelopes.push({
            type: 'takeover',
            id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
            from: pending.fromName,
            to: pending.targetName,
            capability: pending.capability ?? '',
            ts: pending.timestamp,
          })
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

export async function fetchInbox(baseUrl: string, name: string): Promise<Envelope[]> {
  try {
    const res = await fetch(`${baseUrl}/inbox?name=${encodeURIComponent(name)}`)
    if (!res.ok) return []
    const data = (await res.json()) as { ok: boolean; envelopes: Envelope[] }
    return data.envelopes ?? []
  } catch {
    return []
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
