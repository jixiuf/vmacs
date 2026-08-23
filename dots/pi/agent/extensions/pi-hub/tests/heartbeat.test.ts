import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { wsFrame, WS_OP, startCoordinatorServer, connectCoordinatorWS, listActiveClients } from '../src/transport.js'
import { EnvelopeQueue } from '../src/queue.js'
import type { Envelope } from '../src/types.js'

const TEST_STATE_DIR = fs.mkdtempSync(path.join(os.tmpdir(), 'pi-hub-hb-test-'))
process.env.PI_HUB_STATE_DIR = TEST_STATE_DIR

// 缩短 TTL 与心跳间隔，让「登记被心跳刷新」可在秒级内观测
process.env.PI_HUB_ACTIVE_TTL_MS = '1000'
process.env.PI_HUB_WS_HEARTBEAT_MS = '250'

afterAll(() => {
  try { fs.rmSync(TEST_STATE_DIR, { recursive: true, force: true }) } catch { /* ignore */ }
})

// 回归测试：客户端连接后不做任何应用层动作，仅靠服务器主动心跳
// （服务器发协议层 PING → 标准 WebSocket 客户端自动回 PONG → 服务器刷新活跃登记），
// 保证活跃登记不会因 TTL 超时被误清理（实例持续出现在 /instances）。
describe('服务器主动心跳保持活跃登记（回归：文本帧 ping 曾被忽略导致 TTL 误清理）', () => {
  const port = 18090
  const queue = new EnvelopeQueue()
  let server: ReturnType<typeof startCoordinatorServer>
  let client: ReturnType<typeof connectCoordinatorWS>

  beforeAll(async () => {
    server = startCoordinatorServer(port, { name: 'hb-server', pid: 1, cwd: '/tmp', host: 'localhost' }, [], queue)
    await new Promise((r) => setTimeout(r, 200))
    client = connectCoordinatorWS(`http://127.0.0.1:${port}`, 'hb-client', 'localhost', () => {})
    await new Promise((r) => setTimeout(r, 400))
  })

  afterAll(async () => {
    client.close()
    await new Promise((r) => setTimeout(r, 200))
    server.closeAllConnections?.()
    await new Promise<void>((r) => {
      server.close(() => r())
      setTimeout(r, 1500)
    })
  })

  it('TTL(1s) 内多次心跳间隔(250ms)后，活跃登记仍存在', async () => {
    // 连接后活跃登记已建立
    expect(listActiveClients()).toContain('hb-client')
    // 等待超过 1 个 TTL（期间服务器应已多次发 PING → 客户端自动回 PONG 刷新登记）
    await new Promise((r) => setTimeout(r, 1300))
    expect(listActiveClients()).toContain('hb-client')
  })

  it('空闲客户端仍可收到服务器 PING 帧（协议层心跳存在）', async () => {
    // 用原始 socket 探测服务器是否真的发协议层 PING：连接后等待心跳周期，应收到 PING 帧
    const net = await import('node:net')
    const key = Buffer.from('hb-probe-key-1234567890').toString('base64')
    const sock = net.createConnection({ port, host: '127.0.0.1' })
    const received: number[] = []
    sock.on('data', (buf: Buffer) => {
      // 服务器发帧不带 mask；探测 PING(opcode 0x9) 即可
      if (buf.length >= 2 && (buf[0] & 0x0f) === WS_OP.PING) received.push(buf[0] & 0x0f)
    })
    await new Promise<void>((resolve, reject) => {
      sock.once('connect', () => {
        sock.write(
          `GET /ws?name=hb-probe&host=localhost HTTP/1.1\r\n` +
          `Host: 127.0.0.1:${port}\r\n` +
          `Upgrade: websocket\r\n` +
          `Connection: Upgrade\r\n` +
          `Sec-WebSocket-Key: ${key}\r\n` +
          `Sec-WebSocket-Version: 13\r\n\r\n`,
        )
        setTimeout(resolve, 1000)
      })
      sock.once('error', reject)
    })
    expect(received.length).toBeGreaterThan(0)
    sock.destroy()
  })
})
