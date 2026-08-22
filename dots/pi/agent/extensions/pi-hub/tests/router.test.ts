import { describe, it, expect } from 'vitest'
import { Router } from '../src/router.js'
import type { IGateway, InboundMessage } from '../src/types.js'

// 集成冒烟：Router 命令分发 → 命令回复 / 渠道回调
describe('Router 渠道入站路由', () => {
  it('协调命令命中 → 回复走 gateway.send，不回调渠道', async () => {
    let sent: string | null = null
    let userMsgHandled = false
    const gw: IGateway = {
      kind: 'wechat',
      capabilities: { text: true, image: true, file: true, voice: true },
      connect: async () => {},
      disconnect: async () => {},
      onInbound: () => {},
      send: async (_t, m) => { sent = m.text ?? null },
      fetchAttachment: async () => null,
      handleUserMessage: async () => { userMsgHandled = true },
    }
    const router = new Router({
      handleCommand: async (text) => (text === '/instances' ? '实例列表：\n1. home' : null),
      handleMessage: async () => true,
      handleTakeover: () => {},
      getGateway: () => gw,
    })
    router.routeInbound({ id: '1', channel: 'wechat', userId: 'u', text: '/instances', ts: Date.now() })
    await new Promise((r) => setTimeout(r, 50))
    expect(sent).toContain('实例列表')
    expect(userMsgHandled).toBe(false)
  })

  it('非命令 → 回调渠道 handleUserMessage', async () => {
    let handled: InboundMessage | null = null
    const gw: IGateway = {
      kind: 'wechat',
      capabilities: { text: true, image: true, file: true, voice: true },
      connect: async () => {},
      disconnect: async () => {},
      onInbound: () => {},
      send: async () => {},
      fetchAttachment: async () => null,
      handleUserMessage: async (m) => { handled = m },
    }
    const router = new Router({
      handleCommand: async () => null,
      handleMessage: async () => true,
      handleTakeover: () => {},
      getGateway: () => gw,
    })
    router.routeInbound({ id: '2', channel: 'wechat', userId: 'u', text: '你好', ts: Date.now() })
    await new Promise((r) => setTimeout(r, 50))
    expect(handled?.text).toBe('你好')
  })

  it('渠道无 handleUserMessage → 默认投递 handleMessage', async () => {
    let delivered = false
    const gw: IGateway = {
      kind: 'feishu',
      capabilities: { text: true, image: false, file: false, voice: false },
      connect: async () => {},
      disconnect: async () => {},
      onInbound: () => {},
      send: async () => {},
      fetchAttachment: async () => null,
    }
    const router = new Router({
      handleCommand: async () => null,
      handleMessage: async () => { delivered = true; return true },
      handleTakeover: () => {},
      getGateway: () => gw,
    })
    router.routeInbound({ id: '3', channel: 'feishu', userId: 'u', text: 'hello', ts: Date.now() })
    await new Promise((r) => setTimeout(r, 50))
    expect(delivered).toBe(true)
  })
})
