// ============================================================================
// Router：入站消息分类路由（命令 / 接管 / 普通消息）
// 从渠道（wechat remote-commands）中抽出，任何渠道执行结果一致
// ============================================================================

import type { InboundMessage } from './types.js'
import type { Envelope } from './types.js'

export interface RouterDeps {
  /** 斜杠命令/中文别名执行；返回回复文本（null 表示非命令，继续走普通消息） */
  handleCommand: (text: string, userId: string, channel: string) => Promise<string | null>
  /** 普通消息投递给 agent（返回是否已消费） */
  handleMessage: (m: InboundMessage) => Promise<boolean>
  /** 接管请求处理 */
  handleTakeover: (env: Extract<Envelope, { type: 'takeover' }>) => void
}

export class Router {
  constructor(private readonly deps: RouterDeps) {}

  /** 渠道入站（IGateway.onInbound 注册此方法） */
  routeInbound(m: InboundMessage): void {
    void (async () => {
      // 命令：斜杠 / 中文别名 / 语音容错
      if (m.text) {
        const reply = await this.deps.handleCommand(m.text, m.userId, m.channel)
        if (reply !== null) {
          // 命令结果由 gateway 发送（通过 hub 的出站通道）
          this.deps.handleMessage({ ...m, text: undefined } as InboundMessage).catch(() => {})
          this.sendCommandReply(m, reply)
          return
        }
      }
      await this.deps.handleMessage(m)
    })().catch(() => {})
  }

  /** envelope 入站（跨实例：message/command/takeover/lock/broadcast） */
  routeEnvelope(env: Envelope): void {
    switch (env.type) {
      case 'takeover':
        this.deps.handleTakeover(env)
        break
      default:
        // message/command/lock/broadcast 由具体模块处理
        break
    }
  }

  private sendCommandReply(m: InboundMessage, reply: string): void {
    // 命令回复走出站通道：由装配方提供 gateway 映射
    this.onCommandReply?.(m, reply)
  }

  onCommandReply: ((m: InboundMessage, reply: string) => void) | null = null
}
