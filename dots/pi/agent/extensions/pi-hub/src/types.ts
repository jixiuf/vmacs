// ============================================================================
// pi-hub 领域类型：IGateway 契约、Envelope 传输协议、实例/接管/消息模型
// 渠道（wechat/feishu）只依赖本文件类型，不依赖任何协调实现
// ============================================================================

// --- 实例 ---

export interface InstanceInfo {
  name: string
  pid: number
  cwd: string
  sessionId: string
  lastSeen: number
  host?: string
}

// --- 接管请求 ---

export interface TakeoverRequest {
  targetName: string
  targetPid: number
  fromName: string
  /** 业务标识（如 wechat 表示微信接管；空为通用接管） */
  capability?: string
  payload?: unknown
  timestamp: number
}

// --- 协调消息（实例间普通消息） ---

export interface CoordinatorMessage {
  id: string
  from: string
  to: string
  text: string
  timestamp: number
}

// --- 全局锁 ---

export interface GlobalLock {
  name: string
  pid: number
  capability?: string
  lastSeen: number
}

// ============================================================================
// IGateway — IM 渠道唯一契约
// 新增渠道（feishu 等）= 实现一个 IGateway 文件 + 注册到 hub
// ============================================================================

export interface GatewayAttachment {
  kind: 'image' | 'file' | 'voice'
  /** 渠道自管的附件引用（URL / 加密参数 / 文件 id 等），hub 不感知 */
  ref: unknown
}

export interface InboundMessage {
  /** 渠道原始消息 id（幂等去重键） */
  id: string
  /** 渠道标识：'wechat' | 'feishu' */
  channel: string
  /** 会话对象（微信 userId / 飞书 open_id） */
  userId: string
  text?: string
  attachments?: GatewayAttachment[]
  ts: number
}

export interface OutboundMessage {
  text?: string
  attachments?: { kind: 'image' | 'file'; path: string; name?: string }[]
}

export interface GatewayCapabilities {
  text: boolean
  image: boolean
  file: boolean
  voice: boolean
}

export interface IGateway {
  readonly kind: string
  readonly capabilities: GatewayCapabilities
  /** 登录/建立长连接 */
  connect(): Promise<void>
  disconnect(): Promise<void>
  /** 注册入站处理器（长连接/轮询/Webhook 归一化为领域事件） */
  onInbound(h: (m: InboundMessage) => void): void
  /** 出站：发送领域消息，协议细节渠道自管 */
  send(target: string, m: OutboundMessage): Promise<void>
  /** 附件下载（供 hub 需要时取内容，如图片喂给 agent） */
  fetchAttachment(ref: unknown): Promise<Buffer | null>
  /**
   * 渠道自有消息处理（可选）：hub 判定为非协调命令的入站消息回调给渠道，
   * 由渠道完成会话命令（/model /status 等需 pi 上下文）、问卷拦截、入队投递。
   * 不实现时 hub 走默认投递（直接注入 agent）。
   */
  handleUserMessage?(m: InboundMessage): void | Promise<void>
  /**
   * 渠道是否正在等待某用户的答案（问卷/问题拦截中，可选）。
   * hub 在宽松命令匹配（数字/实例名 → use）前检查：等待答案时用户发的数字可能真是答案，
   * 不应作为切换命令消费。不实现时按 false 处理。
   */
  isAwaitingAnswer?(userId: string): boolean
}

// ============================================================================
// Envelope — 跨实例统一传输协议
// 取代旧的 takeover.json / coordinator-messages.json / coordinator-lock.json 三套机制
// ============================================================================

export type Envelope =
  | { type: 'message';   id: string; from: string; to: string; text: string; ts: number }
  | { type: 'command';   id: string; from: string; to: string; command: string; ts: number }
  | { type: 'takeover';  id: string; from: string; to: string; capability: string; ts: number }
  | { type: 'lock';      id: string; from: string; capability: string; ts: number }
  | { type: 'broadcast'; id: string; from: string; command: string; ts: number }

export function makeEnvelopeId(): string {
  return `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`
}
