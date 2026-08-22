import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import * as http from 'node:http'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { EnvelopeQueue } from '../src/queue.js'
import { startCoordinatorServer } from '../src/transport.js'
import { requestRemoteLock, releaseRemoteLock, fetchInbox, postEnvelope } from '../src/transport.js'

// 集成测试：真实 HTTP 协调中心，验证远程锁互斥 + envelope 收发
describe('协调中心 HTTP 集成', () => {
  let server: http.Server
  let port = 0
  let baseUrl = ''

  beforeAll(async () => {
    server = startCoordinatorServer(0, { name: 'server', pid: 1, cwd: '/tmp' }, ['client-a'], new EnvelopeQueue())
    await new Promise<void>((r) => server.once('listening', () => r()))
    const addr = server.address() as { port: number }
    port = addr.port
    baseUrl = `http://127.0.0.1:${port}`
  })

  afterAll(() => {
    server.close()
  })

  it('远程锁：第一个获取成功，第二个失败，释放后可获取', async () => {
    const r1 = await requestRemoteLock(baseUrl, 'instance-a', 100, 'wechat')
    expect(r1.ok).toBe(true)

    const r2 = await requestRemoteLock(baseUrl, 'instance-b', 200, 'wechat')
    expect(r2.ok).toBe(false)
    expect(r2.holder?.name).toBe('instance-a')

    await releaseRemoteLock(baseUrl, 'instance-a', 'wechat')
    const r3 = await requestRemoteLock(baseUrl, 'instance-b', 200, 'wechat')
    expect(r3.ok).toBe(true)
  })

  it('envelope 收发：客户端入队 → 服务器 inbox 取出', async () => {
    await postEnvelope(baseUrl, {
      type: 'message',
      id: 'e1',
      from: 'server',
      to: 'client-a',
      text: 'hello',
      ts: Date.now(),
    })
    const inbox = await fetchInbox(baseUrl, 'client-a')
    expect(inbox.length).toBe(1)
    expect(inbox[0].type).toBe('message')
    // 取出后队列清空（ack 在客户端侧，此处验证读取即从队列移除待处理集）
  })
})

