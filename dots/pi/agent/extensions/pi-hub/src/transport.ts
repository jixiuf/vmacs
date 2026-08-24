// ============================================================================
// 传输层：协调中心 HTTP 服务 + 客户端拉取（保留现有 8089 模式，行为兼容）
// 统一 Envelope 协议，取代旧的 takeover/messages/lock 三套端点
// ============================================================================

import * as http from 'node:http'
import type { Socket } from 'node:net'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import * as crypto from 'node:crypto'
import { execFile } from 'node:child_process'
import type { Envelope, InstanceInfo, TakeoverRequest } from './types.js'
import { EnvelopeQueue } from './queue.js'
import { preassignLock, coordinatorTryLock, coordinatorReleaseLock, getGlobalLockHolder } from './lock.js'

// ============================================================================
// WebSocket（手写帧，无依赖）：协调中心 ↔ 客户端双向通信，替代 2s HTTP 轮询
// ============================================================================

const WS_GUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'

export const WS_OP = { TEXT: 0x1, CLOSE: 0x8, PING: 0x9, PONG: 0xa } as const

/** 服务端发帧：无 mask（客户端→服务端帧才要求 mask） */
export function wsFrame(opcode: number, payload: Buffer | string): Buffer {
  const data = Buffer.isBuffer(payload) ? payload : Buffer.from(payload, 'utf8')
  const len = data.length
  let header: Buffer
  if (len < 126) {
    header = Buffer.alloc(2)
    header[1] = len
  } else if (len < 65536) {
    header = Buffer.alloc(4)
    header[1] = 126
    header.writeUInt16BE(len, 2)
  } else {
    header = Buffer.alloc(10)
    header[1] = 127
    header.writeBigUInt64BE(BigInt(len), 2)
  }
  header[0] = 0x80 | (opcode & 0x0f)
  return Buffer.concat([header, data])
}

/** 解析客户端帧（带 mask）。缓冲不足返回 null（等待更多数据）。 */
export function wsParseFrame(buf: Buffer): { opcode: number; payload: Buffer; consumed: number } | null {
  if (buf.length < 2) return null
  const fin = (buf[0] & 0x80) !== 0
  const opcode = buf[0] & 0x0f
  const masked = (buf[1] & 0x80) !== 0
  let len = buf[1] & 0x7f
  let offset = 2
  if (len === 126) {
    if (buf.length < 4) return null
    len = buf.readUInt16BE(2)
    offset = 4
  } else if (len === 127) {
    if (buf.length < 10) return null
    len = Number(buf.readBigUInt64BE(2))
    offset = 10
  }
  const maskLen = masked ? 4 : 0
  if (buf.length < offset + maskLen + len) return null
  const mask = masked ? buf.subarray(offset, offset + 4) : null
  const payload = Buffer.from(buf.subarray(offset + maskLen, offset + maskLen + len))
  if (mask) {
    for (let i = 0; i < payload.length; i++) payload[i] ^= mask[i % 4]
  }
  return { opcode, payload, consumed: offset + maskLen + len }
}

/**
 * 客户端 WS 连接（node 22+ 原生 WebSocket）：接收服务器推送的 envelope + 发送 envelope。
 * 自动重连（指数退避）+ 30s 心跳。
 */
export interface WsClientHandle {
  /** 投递 envelope 到协调中心；WS 未连接返回 false（调用方回退 HTTP） */
  sendEnvelope(env: Envelope): boolean
  close(): void
}

export function connectCoordinatorWS(
  baseUrl: string,
  name: string,
  host: string,
  onEnvelope: (env: Envelope) => void,
  onStatus?: (connected: boolean) => void,
  sessionName?: string,
): WsClientHandle {
  const url = new URL(baseUrl)
  const wsUrl = `ws://${url.host}/ws?name=${encodeURIComponent(name)}&host=${encodeURIComponent(host)}${sessionName ? `&session=${encodeURIComponent(sessionName)}` : ''}`
  let ws: WebSocket | null = null
  let closed = false
  let retry = 0

  function connect(): void {
    if (closed) return
    let socket: WebSocket
    try {
      socket = new WebSocket(wsUrl)
    } catch {
      if (!closed) setTimeout(connect, 3000)
      return
    }
    socket.onopen = () => { retry = 0; onStatus?.(true) }
    socket.onmessage = (ev) => {
      try {
        const msg = JSON.parse(String(ev.data)) as { type?: string; env?: Envelope }
        if (msg.type === 'envelope' && msg.env) onEnvelope(msg.env)
      } catch { /* 忽略坏消息 */ }
    }
    socket.onclose = () => {
      onStatus?.(false)
      if (ws === socket) ws = null
      if (!closed) {
        const delay = Math.min(1000 * 2 ** retry, 15000)
        retry++
        setTimeout(connect, delay)
      }
    }
    socket.onerror = () => { try { socket.close() } catch { /* ignore */ } }
    ws = socket
  }

  connect()
  return {
    sendEnvelope(env) {
      if (ws?.readyState === WebSocket.OPEN) {
        try { ws.send(JSON.stringify({ type: 'envelope', env })); return true } catch { /* ignore */ }
      }
      return false
    },
    close() {
      closed = true
      try { ws?.close() } catch { /* ignore */ }
    },
  }
}


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
/** 客户端实例名 → 会话标题（/ws 连接上报，/instances 展示） */
const clientSessionNames = new Map<string, string>()
const ACTIVE_CLIENT_TTL_MS = Number(process.env.PI_HUB_ACTIVE_TTL_MS) || 30_000

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
        const pendingItems = queue.dequeue(name)
        const envelopes = pendingItems.map(({ env }) => env)
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
        // 队列消息已返回给轮询客户端：视为投递成功即 ack（防轮询重取/重启重投导致重复）
        for (const { ack } of pendingItems) ack()
        return
      }

      // 客户端入队：message / command / takeover / broadcast / lock
      // 目标 WS 在线 → 直接推送（低延迟）；否则入队等 WS 连接
      if (req.method === 'POST' && url.pathname === '/envelope') {
        let body = ''
        for await (const chunk of req) body += chunk
        const env = JSON.parse(body) as Envelope
        // 接管请求到达：无条件预占锁（当前持有者心跳失败自动让位）
        if (env.type === 'takeover') {
          preassignLock(env.to, 0, env.capability)
        }
        deliverEnvelope(env)
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
          .filter((n) => n !== local.name && wsClients.has(n))
          .map((n) => ({ name: n, pid: 0, cwd: '', host: clientHosts.get(n), sessionName: clientSessionNames.get(n) }))
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
    } catch (err) {
      // 服务器端异常必须可观测（曾因静默 500 导致 POST /envelope 排查困难）
      console.error('[pi-hub:http] 500', (err as Error).message)
      res.writeHead(500)
      res.end('error')
    }
  })
  // --- WebSocket：客户端长连接（替代 /inbox 轮询），服务器即时推送 ---
  const wsClients = new Map<string, Socket>()

  /** 投递 envelope：目标 WS 在线 → 直接推送；否则入队等连接 */
  function deliverEnvelope(env: Envelope): void {
    // lock/broadcast 无 to 目标，直接入队（由协调中心/广播消费）
    const to = (env as { to?: string }).to
    if (!to) {
      queue.enqueue(env)
      return
    }
    const targetSocket = wsClients.get(to)
    if (targetSocket) {
      try {
        targetSocket.write(wsFrame(WS_OP.TEXT, JSON.stringify({ type: 'envelope', env })))
        return
      } catch { /* 写入失败则入队 */ }
    }
    queue.enqueue(env)
  }

  server.on('upgrade', (req, socket, _head) => {
    const sock = socket as unknown as Socket
    try {
      const url = new URL(req.url ?? '/', 'http://localhost')
      if (url.pathname !== '/ws') { sock.destroy(); return }
      const name = url.searchParams.get('name') ?? ''
      const host = url.searchParams.get('host') ?? ''
      const session = url.searchParams.get('session') ?? ''
      const key = req.headers['sec-websocket-key'] as string | undefined
      if (!key || !name) { sock.destroy(); return }
      if (session) clientSessionNames.set(name, session)
      else clientSessionNames.delete(name)
      // TCP keepalive：内核探测死连接（进程突然退出时更快触发 close/error）
      sock.setKeepAlive(true, 10000)
      const accept = crypto.createHash('sha1').update(key + WS_GUID).digest('base64')
      sock.write(
        'HTTP/1.1 101 Switching Protocols\r\n' +
        'Upgrade: websocket\r\n' +
        'Connection: Upgrade\r\n' +
        `Sec-WebSocket-Accept: ${accept}\r\n\r\n`,
      )
      markActiveClient(name, host)
      if (session) clientSessionNames.set(name, session)
      else clientSessionNames.delete(name)

      // 连接即投递积压（本地队列 + 远程接管）
      const pendingItems = queue.dequeue(name)
      // 队列中的点对点消息：推送成功即 ack（ack 后进入 dedup 集并从队列删除）。
      // 不 ack 会导致消息滞留队列，每次重连/重启后重复投递（已处理消息重复进入会话）。
      for (const { env, ack } of pendingItems) {
        try {
          sock.write(wsFrame(WS_OP.TEXT, JSON.stringify({ type: 'envelope', env })))
          ack()
        } catch { break }
      }
      // 远程接管请求（即时构造，非队列消息）：单独推送
      const byName = remoteQueue.get(name)
      const byHost = host ? [...remoteQueue.entries()].find(([k, r]) => r.targetName === host || clientHosts.get(r.targetName) === host) : undefined
      const entry = byName ? [name, byName] as const : byHost
      if (entry) {
        const [qKey, p] = entry
        if (p && (p.targetPid === 0 || p.targetPid === 0)) {
          remoteQueue.delete(qKey)
          const takeoverEnv = {
            type: 'takeover',
            id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
            from: p.fromName,
            to: p.targetName,
            capability: p.capability ?? '',
            ts: p.timestamp,
          } as Envelope
          try { sock.write(wsFrame(WS_OP.TEXT, JSON.stringify({ type: 'envelope', env: takeoverEnv }))) } catch { /* ignore */ }
        }
      }

      const old = wsClients.get(name)
      if (old && old !== sock) { try { old.destroy() } catch { /* ignore */ } }
      wsClients.set(name, sock)

      let buffer = Buffer.alloc(0)
      // 客户端 TCP 半关闭（进程退出只发 FIN）→ 回关写侧，触发完整 close → 即时清理
      sock.on('end', () => { try { sock.end() } catch { /* ignore */ } })
      sock.on('data', (chunk) => {
        buffer = Buffer.concat([buffer, chunk])
        for (;;) {
          const parsed = wsParseFrame(buffer)
          if (!parsed) break
          buffer = buffer.subarray(parsed.consumed)
          try {
            if (parsed.opcode === WS_OP.PING) {
              // 客户端主动 ping（协议层）：刷新活跃登记 + 回 pong
              markActiveClient(name, host)
              if (session) clientSessionNames.set(name, session)
              else clientSessionNames.delete(name)
              sock.write(wsFrame(WS_OP.PONG, Buffer.alloc(0)))
              continue
            }
            if (parsed.opcode === WS_OP.PONG) {
              // 心跳响应：服务器主动 PING 后，标准 WebSocket 客户端自动回 PONG → 刷新活跃登记
              markActiveClient(name, host)
              continue
            }
            if (parsed.opcode === WS_OP.CLOSE) {
              // 回 close 帧并关闭连接（触发 close 事件 → activeClients 清理）
              try { sock.write(wsFrame(WS_OP.CLOSE, Buffer.alloc(0))) } catch { /* ignore */ }
              sock.end()
              continue
            }
            if (parsed.opcode !== WS_OP.TEXT) continue
            const msg = JSON.parse(parsed.payload.toString()) as { type?: string; env?: Envelope }
            if (msg.type === 'envelope' && msg.env) {
              const env = msg.env
              if (env.type === 'takeover') preassignLock(env.to, 0, env.capability)
              deliverEnvelope(env)
            }
          } catch (err) {
            console.error('[pi-hub:ws] 帧处理异常', (err as Error).message)
          }
        }
      })
      sock.on('close', () => {
        if (wsClients.get(name) === sock) {
          wsClients.delete(name)
          // 断开即清理活跃登记（实例名不再出现在 /instances 客户端列表）
          activeClients.delete(name)
          clientHosts.delete(name)
          clientSessionNames.delete(name)
        }
      })
      sock.on('error', () => {
        if (wsClients.get(name) === sock) {
          wsClients.delete(name)
          activeClients.delete(name)
          clientHosts.delete(name)
          clientSessionNames.delete(name)
        }
      })
    } catch (err) {
      console.error('[pi-hub:ws] upgrade 处理异常', (err as Error).message)
    }
  })

  // TTL 兜底清理：进程突然退出（process.exit/RST）不触发 close 事件时，靠心跳超时清理残留登记
  // 关键：删除登记前先 destroy 假死连接（TCP 半开/PONG 停）。否则客户端连接仍 ESTAB 认为在线、
  // 不重连，但服务器 wsClients 已删 → deliverEnvelope 入队不推送 → 客户端永久“假在线”收不到消息。
  const activePrune = setInterval(() => {
    const now = Date.now()
    for (const [k, ts] of [...activeClients]) {
      if (now - ts > ACTIVE_CLIENT_TTL_MS) {
        activeClients.delete(k)
        clientHosts.delete(k)
        clientSessionNames.delete(k)
        const stale = wsClients.get(k)
        if (stale) {
          try {
            stale.destroy() // 触发客户端 close → 重连 → 重新握手注册（恢复可见性）
          } catch {
            // ignore
          }
        }
        wsClients.delete(k)
      }
    }
  }, 30000)

  // 服务器主动心跳：定期向所有 WS 客户端发协议层 PING 帧。标准 WebSocket 客户端
  // （Node/浏览器）收到 PING 后自动回 PONG（协议层行为，无需应用代码），服务器以
  // PONG 刷新活跃登记。TTL(30s) 大于心跳间隔(15s)，正常连接不会被误清理。
  const wsHeartbeat = setInterval(() => {
    for (const [, sock] of [...wsClients]) {
      try { sock.write(wsFrame(WS_OP.PING, Buffer.alloc(0))) } catch { /* ignore */ }
    }
  }, Number(process.env.PI_HUB_WS_HEARTBEAT_MS) || 15000)
  server.on('close', () => {
    clearInterval(activePrune)
    clearInterval(wsHeartbeat)
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

export function sshExec(target: string, port: number | undefined, command: string, timeoutMs = 15000): Promise<string> {
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
