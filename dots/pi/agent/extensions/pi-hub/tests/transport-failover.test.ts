import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import * as http from 'node:http'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { EnvelopeQueue } from '../src/queue.js'
import { startCoordinatorServer } from '../src/transport.js'
import { requestRemoteLock, releaseRemoteLock } from '../src/transport.js'

// 模拟"服务器停 → 客户端降级接管"场景：
// 1. 协调中心在线时，A 持有锁，B 获取失败
// 2. 协调中心停止（服务器 pi 停）后，B 请求锁 → unreachable=true（可降级接管）
describe('协调中心不可达 → 降级接管', () => {
  let server: http.Server
  let port = 0
  let baseUrl = ''

  beforeAll(async () => {
    server = startCoordinatorServer(0, { name: 'server', pid: 1, cwd: '/tmp' }, ['client-b'], new EnvelopeQueue())
    await new Promise<void>((r) => server.once('listening', () => r()))
    port = (server.address() as { port: number }).port
    baseUrl = `http://127.0.0.1:${port}`
  })

  it('服务器在线：A 持锁，B 被拒', async () => {
    const a = await requestRemoteLock(baseUrl, 'instance-a', 100, 'wechat')
    expect(a.ok).toBe(true)
    const b = await requestRemoteLock(baseUrl, 'instance-b', 200, 'wechat')
    expect(b.ok).toBe(false)
    expect(b.unreachable).toBeUndefined()
    await releaseRemoteLock(baseUrl, 'instance-a', 'wechat')
  })

  it('服务器停止：B 请求锁 → unreachable=true（可降级本地接管）', async () => {
    await new Promise<void>((r) => server.close(() => r()))
    // 端口已关闭，fetch 抛连接拒绝 → unreachable=true
    const b = await requestRemoteLock(baseUrl, 'instance-b', 200, 'wechat')
    expect(b.ok).toBe(false)
    expect(b.unreachable).toBe(true)
  }, 10_000)
})
