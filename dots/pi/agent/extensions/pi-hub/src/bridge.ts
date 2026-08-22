// ============================================================================
// SessionBridge：渠道消息 ↔ pi.sendUserMessage 的唯一桥
// 取代旧的 globalThis.__PI_COORDINATOR__.onMessage/onTakeover 桥（加载顺序敏感）
// hub 装配时创建一次，渠道通过 gateway 注册，不再跨扩展握手
// ============================================================================

import type { InboundMessage, OutboundMessage, TakeoverRequest } from './types.js'

// pi 扩展 API 的最小结构化子集（不直接依赖 @mariozechner 类型，避免 peer dep 解析问题）
export interface BridgePiLike {
  sendUserMessage: (
    content: string,
    opts?: { deliverAs?: string; expandPromptTemplates?: boolean },
  ) => void | Promise<unknown>
}

export interface BridgeDeps {
  pi: BridgePiLike
  /** 渠道入站消息 → agent 会话（文本/图片），返回 Promise 便于 .catch */
  deliverToAgent: (m: InboundMessage) => Promise<unknown>
  /** 接管请求处理（由 hub 的 lock/router 提供） */
  onTakeover?: (req: TakeoverRequest) => void
}

export class SessionBridge {
  private readonly deliverToAgent: (m: InboundMessage) => Promise<unknown>
  private readonly onTakeover?: (req: TakeoverRequest) => void

  /** 渠道入站处理器（由 gateway 注册） */
  private inboundHandler: ((m: InboundMessage) => void) | null = null

  constructor(deps: BridgeDeps) {
    this.deliverToAgent = deps.deliverToAgent
    this.onTakeover = deps.onTakeover
  }

  /** gateway 调用：提交入站消息 */
  handleInbound(m: InboundMessage): void {
    this.inboundHandler?.(m)
  }

  /** hub 装配时注册实际处理（router） */
  setInboundHandler(h: (m: InboundMessage) => void): void {
    this.inboundHandler = h
  }

  /** hub 收到接管请求时调用 */
  notifyTakeover(req: TakeoverRequest): void {
    this.onTakeover?.(req)
  }
}
