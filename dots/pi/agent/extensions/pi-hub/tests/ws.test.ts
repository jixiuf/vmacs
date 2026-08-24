import { describe, it, expect, afterAll, beforeAll } from 'vitest'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { wsFrame, wsParseFrame, WS_OP, startCoordinatorServer, connectCoordinatorWS, wsRetryDelay } from '../src/transport.js'
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

describe('WS 重连退避 wsRetryDelay（防同名竞争重连风暴）', () => {
  it('从未 OPEN（alive=-1）→ 保持正常指数退避递增', () => {
    expect(wsRetryDelay(-1, 0, Infinity)).toBe(1000)
    expect(wsRetryDelay(-1, 2, Infinity)).toBe(4000)
  })

  it('稳定存活（≥10s）且距上次断开久（≥30s）→ 重置退避到 1s', () => {
    expect(wsRetryDelay(20_000, 7, 60_000)).toBe(1000)
    expect(wsRetryDelay(10_000, 3, 45_000)).toBe(1000)
  })

  it('快速失败（存活 <3s）→ 强制提升退避下限 ≥8s', () => {
    expect(wsRetryDelay(1000, 0, Infinity)).toBe(8000)
    expect(wsRetryDelay(500, 1, Infinity)).toBe(8000)
    expect(wsRetryDelay(2999, 0, Infinity)).toBe(8000)
  })

  it('关键：稳定运行后被踢（alive 很大但距上次断开 <30s）→ 竞争节奏，仍强制大退避', () => {
    // 复现真实风暴：连接稳定 76s 后被同名竞争源踢掉，仅看 alive 会误重置 retry
    expect(wsRetryDelay(76_000, 0, 1000)).toBe(8000)
    expect(wsRetryDelay(50_000, 1, 5000)).toBe(8000)
    expect(wsRetryDelay(76_000, 4, 10_000)).toBe(15000)
  })

  it('快速失败持续 → 退避指数增长至 15s 上限（风暴抑制）', () => {
    expect(wsRetryDelay(500, 3, 1000)).toBe(8000)
    expect(wsRetryDelay(500, 4, 1000)).toBe(15000)
    expect(wsRetryDelay(500, 9, 1000)).toBe(15000)
  })

  it('介于稳定与快速失败之间（3s~10s）且无竞争节奏 → 保持当前退避', () => {
    expect(wsRetryDelay(5000, 2, 60_000)).toBe(4000)
    expect(wsRetryDelay(5000, 5, 60_000)).toBe(15000)
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
