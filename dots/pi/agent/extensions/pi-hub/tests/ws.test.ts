import { describe, it, expect, afterAll, beforeAll } from 'vitest'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { wsFrame, wsParseFrame, WS_OP, startCoordinatorServer, connectCoordinatorWS } from '../src/transport.js'
import { EnvelopeQueue } from '../src/queue.js'
import type { Envelope } from '../src/types.js'

const TEST_STATE_DIR = fs.mkdtempSync(path.join(os.tmpdir(), 'pi-hub-ws-test-'))
process.env.PI_HUB_STATE_DIR = TEST_STATE_DIR

afterAll(() => {
  try { fs.rmSync(TEST_STATE_DIR, { recursive: true, force: true }) } catch { /* ignore */ }
})

describe('WebSocket 帧编解码（手写，无依赖）', () => {
  it('服务端发帧：无 mask，text', () => {
    const frame = wsFrame(WS_OP.TEXT, 'hello')
    expect(frame[0]).toBe(0x81) // FIN + text
    expect(frame[1]).toBe(5) // 短长度
    expect(frame.subarray(2).toString()).toBe('hello')
  })

  it('服务端发帧：126 长度分支', () => {
    const data = 'x'.repeat(200)
    const frame = wsFrame(WS_OP.TEXT, data)
    expect(frame[1]).toBe(126)
    expect(frame.readUInt16BE(2)).toBe(200)
    expect(frame.length).toBe(4 + 200)
  })

  it('解析客户端帧（带 mask），异或还原', () => {
    const payload = Buffer.from('test-masked')
    const mask = Buffer.from([0x11, 0x22, 0x33, 0x44])
    const masked = Buffer.from(payload.map((b, i) => b ^ mask[i % 4]))
    const frame = Buffer.concat([Buffer.from([0x81, 0x80 | payload.length]), mask, masked])
    const parsed = wsParseFrame(frame)
    expect(parsed).not.toBeNull()
    expect(parsed!.opcode).toBe(WS_OP.TEXT)
    expect(parsed!.payload.toString()).toBe('test-masked')
    expect(parsed!.consumed).toBe(frame.length)
  })

  it('缓冲不足返回 null（等待更多数据）', () => {
    expect(wsParseFrame(Buffer.from([0x81]))).toBeNull()
  })
})

describe('协调中心 WS 双向投递（集成）', () => {
  const port = 18089
  const queue = new EnvelopeQueue()
  let server: ReturnType<typeof startCoordinatorServer>
  const receivedA: Envelope[] = []
  const receivedB: Envelope[] = []
  let clientA: ReturnType<typeof connectCoordinatorWS>
  let clientB: ReturnType<typeof connectCoordinatorWS>

  beforeAll(async () => {
    server = startCoordinatorServer(port, { name: 'testserver', pid: 1, cwd: '/tmp', host: 'localhost' }, [], queue)
    await new Promise((r) => setTimeout(r, 200)) // startCoordinatorServer 内部已 listen
    clientA = connectCoordinatorWS(`http://127.0.0.1:${port}`, 'testA', 'localhost', (env) => receivedA.push(env))
    await new Promise((r) => setTimeout(r, 300))
    clientB = connectCoordinatorWS(`http://127.0.0.1:${port}`, 'testB', 'localhost', (env) => receivedB.push(env))
    await new Promise((r) => setTimeout(r, 300))
  })

  afterAll(async () => {
    clientA.close()
    clientB.close()
    await new Promise((r) => setTimeout(r, 300))
    server.closeAllConnections?.()
    await new Promise<void>((r) => {
      server.close(() => r())
      setTimeout(r, 1500) // 兜底：不阻塞测试退出
    })
  })

  it('A → B 消息即时投递（不经轮询）', async () => {
    clientA.sendEnvelope({ type: 'message', id: 'm1', from: 'testA', to: 'testB', text: 'ping-a', ts: Date.now() })
    await new Promise((r) => setTimeout(r, 300))
    expect(receivedB.length).toBe(1)
    expect(receivedB[0].text).toBe('ping-a')
  })

  it('B → A 命令投递', async () => {
    clientB.sendEnvelope({ type: 'command', id: 'c1', from: 'testB', to: 'testA', command: '/reload', ts: Date.now() })
    await new Promise((r) => setTimeout(r, 300))
    expect(receivedA.length).toBe(1)
    expect(receivedA[0]).toMatchObject({ type: 'command', command: '/reload' })
  })

  it('目标不在线时入队，连接后补投', async () => {
    clientA.sendEnvelope({ type: 'message', id: 'm2', from: 'testA', to: 'testOffline', text: 'later', ts: Date.now() })
    await new Promise((r) => setTimeout(r, 200))
    expect(queue.pendingCount).toBeGreaterThan(0)
    const receivedC: Envelope[] = []
    const clientC = connectCoordinatorWS(`http://127.0.0.1:${port}`, 'testOffline', 'localhost', (env) => receivedC.push(env))
    await new Promise((r) => setTimeout(r, 400))
    expect(receivedC.some((e) => e.text === 'later')).toBe(true)
    clientC.close()
  })
})
