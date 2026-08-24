// ============================================================================
// Router：入站消息分类路由（命令 / 普通消息）+ 跨实例 envelope 分发
// 从渠道（wechat remote-commands）中抽出，任何渠道执行结果一致
// ============================================================================

import type { InboundMessage, Envelope, IGateway } from './types.js'

export interface RouterDeps {
  /** 斜杠命令/中文别名执行；返回回复文本（null 表示非命令，继续走普通消息） */
  handleCommand: (text: string, userId: string, channel: string) => Promise<string | null>
  /** 普通消息投递给 agent（返回是否已消费） */
  handleMessage: (m: InboundMessage) => Promise<boolean>
  /** 渠道注册表（用于命令回复 + 非命令消息回调） */
  getGateway: (channel: string) => IGateway | undefined
  // --- envelope 分发（跨实例：message/command/takeover/broadcast 的实现由装配方提供） ---
  onMessage: (env: Extract<Envelope, { type: 'message' }>) => void
  onCommand: (env: Extract<Envelope, { type: 'command' }>) => void
  onTakeover: (env: Extract<Envelope, { type: 'takeover' }>) => void
  onBroadcast: (env: Extract<Envelope, { type: 'broadcast' }>) => void
}

export class Router {
  constructor(private readonly deps: RouterDeps) {}

  /** 渠道入站（IGateway.onInbound 注册此方法） */
  routeInbound(m: InboundMessage): void {
    void (async () => {
      // 命令：斜杠 / 中文别名 / 语音容错（协调命令由 hub 处理）
      if (m.text) {
        const reply = await this.deps.handleCommand(m.text, m.userId, m.channel)
        if (reply !== null) {
          const gw = this.deps.getGateway(m.channel)
          if (gw) void gw.send(m.userId, { text: reply }).catch(() => {})
          return
        }
      }
      // 非协调命令：优先回调渠道自有处理（会话命令/问卷/入队），否则默认投递
      const gw = this.deps.getGateway(m.channel)
      if (gw?.handleUserMessage) {
        void gw.handleUserMessage(m)
        return
      }
      await this.deps.handleMessage(m)
    })().catch(() => {})
  }

  /** envelope 入站（跨实例：message/command/takeover/lock/broadcast 统一分发） */
  routeEnvelope(env: Envelope): void {
    switch (env.type) {
      case 'message':
        this.deps.onMessage(env)
        break
      case 'command':
        this.deps.onCommand(env)
        break
      case 'takeover':
        this.deps.onTakeover(env)
        break
      case 'broadcast':
        this.deps.onBroadcast(env)
        break
      case 'lock':
        // lock 走独立文件协议
        break
    }
  }
}
