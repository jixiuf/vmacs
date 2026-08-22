# pi 多实例协调架构（v2 设计）

> 状态：设计稿，待 review 后实施
> 范围：pi-hub（协调核心）+ 各 IM 渠道网关（wechat 先行，feishu 后续）

---

## 1. 背景与动机

当前 `pi-coordinator` 与 `pi-wechat-assistant` 存在以下问题（均基于代码走查确认）：

| # | 问题 | 证据 |
|---|------|------|
| 1 | **同一套协调逻辑两份实现** | `pi-wechat-assistant/src/instances.ts`（439 行）与 `pi-coordinator/src/coordinator.ts`（520 行）重复实现 registerInstance / takeover / lock / coordinator server |
| 2 | **渠道内嵌协调逻辑** | wechat 的 index.ts 里有 tryTakeover / tryBroadcast / degraded mode / coordinator bridge，职责混杂 |
| 3 | **消息"读即删"，处理失败即丢** | `consumeLocalMessages` / `GET /messages` 服务器先删后返，无 ack、无去重（已实测复现） |
| 4 | **加载顺序敏感** | 通过 `globalThis.__PI_COORDINATOR__` 桥通信，onTakeover/onMessage 注册依赖扩展加载顺序，已打 3 个补丁仍脆弱 |
| 5 | **命令逻辑与别名表耦合** | `remote-commands.ts`（851 行）命令执行 + 中文别名 + 语音容错正则混在一起，扩展一个渠道要复制全部 |
| 6 | **状态文件分散** | takeover.json / coordinator-messages.json / coordinator-lock.json / instances.json 各管一摊，无统一 envelope |

**目标**：协调逻辑收敛为单一核心（pi-hub），IM 渠道退化为纯协议网关（IGateway），新增 IM 只写一个文件；消息不丢；命令在任意渠道行为一致。

---

## 2. 目标架构

```
┌─────────────────────────── pi-hub（协调核心，单一扩展）───────────────────────────┐
│                                                                                  │
│   registry（实例注册表）    lock（全局锁/接管仲裁）    queue（可靠消息队列）          │
│   router（入站分类）        commands（命令表+别名表）  transport（HTTP/WS）          │
│   bridge（渠道消息 ↔ pi.sendUserMessage）                                          │
│                                                                                  │
│   IGateway 接口（渠道唯一契约）                                                     │
└──────────────────────────────┬───────────────────────────────────────────────────┘
                               │
          ┌────────────────────┼────────────────────┐
          ▼                    ▼                    ▼
   pi-wechat-assistant   pi-feishu-assistant    （未来 IM...）
   WechatGateway         FeishuGateway
   （只做协议收发）        （只做协议收发）
```

**依赖方向**：`gateway → hub`（渠道调用 hub 提交消息），`hub 通过 IGateway 调用渠道`（下发回复/接管）。**渠道永不 import 协调逻辑，hub 永不 import 渠道协议**。

---

## 3. 核心接口

### 3.1 IGateway（渠道唯一契约）

```ts
// pi-hub/src/types.ts
export interface IGateway {
  readonly kind: string                                   // 'wechat' | 'feishu'
  readonly capabilities: { text: boolean; image: boolean; file: boolean; voice: boolean }
  connect(): Promise<void>
  disconnect(): Promise<void>
  // 入站：长连接/轮询/Webhook 归一化为领域事件
  onInbound(h: (m: InboundMessage) => void): void
  // 出站：只发领域消息，协议细节渠道自管
  send(target: string, m: OutboundMessage): Promise<void>
  // 附件下载/上传由渠道自管，暴露领域形态
  fetchAttachment(ref: unknown): Promise<Buffer>
}

export interface InboundMessage {
  id: string                    // 渠道原始 id（幂等去重键）
  channel: string               // 'wechat'
  userId: string                // 会话对象（微信 userId / 飞书 open_id）
  text?: string
  attachments?: { kind: 'image' | 'file' | 'voice'; ref: unknown }[]
  ts: number
}

export interface OutboundMessage {
  text?: string
  attachments?: { kind: 'image' | 'file'; path: string; name?: string }[]
}
```

### 3.2 Envelope（跨实例传输协议，统一）

```ts
export type Envelope =
  | { type: 'message';   id: string; from: string; to: string; text: string; ts: number }
  | { type: 'command';   id: string; from: string; to: string; command: string; ts: number }
  | { type: 'takeover';  id: string; from: string; to: string; capability: string; ts: number }
  | { type: 'lock';      id: string; from: string; capability: string; ts: number }
  | { type: 'broadcast'; id: string; from: string; command: string; ts: number }
```

现有三套文件 + 两套 HTTP 端点收敛为 transport 层四个端点：

| 端点 | 语义 |
|------|------|
| `POST /envelope` | 入队（message/command/takeover/broadcast） |
| `GET /inbox?to=X` | 轮询取出（返回 env + ack 回调，处理成功才删） |
| `GET /registry` | 实例列表（含 hostname） |
| `GET/POST /lock` `POST /unlock` | 全局锁（心跳 + TTL） |

### 3.3 Queue（可靠消息队列，修"读即删"）

```ts
export interface Queue {
  enqueue(e: Envelope): void
  // 取出时返回 ack：处理成功才删除；超时未 ack 自动重投
  dequeue(to: string, timeoutMs: number): Promise<Array<{ env: Envelope; ack: () => void }>>
  // 幂等去重：已处理 id 集合（内存 + 落盘），重投不重复进会话
}
```

存储默认 JSON 文件（零依赖），接口预留 SQLite/Redis 实现。

---

## 4. 目录结构

### 4.1 pi-hub（新，由 pi-coordinator 演化）

```
extensions/pi-hub/
├── index.ts              # 入口：装配 hub、注册工具/TUI 命令/生命周期
└── src/
    ├── types.ts          # IGateway / Envelope / InboundMessage / InstanceInfo / TakeoverRequest
    ├── config.ts         # 单一 schema：实例名/传输模式/渠道开关
    ├── registry.ts       # 实例注册表（合并两处实现）
    ├── lock.ts           # 全局锁 + 心跳 + 接管仲裁（合并两处实现）
    ├── queue.ts          # 可靠消息队列（JSON + ack + 去重 + TTL）
    ├── router.ts         # 入站分类：命令 / 接管 / 普通消息
    ├── commands.ts       # 命令表 + 中文别名 + 语音容错（纯数据 + 通用匹配器）
    ├── transport.ts      # HTTP server/client（保留 8089 模式）+ 预留 WS
    └── bridge.ts         # SessionBridge：渠道消息 ↔ pi.sendUserMessage（唯一桥）
```

### 4.2 pi-wechat-assistant（瘦身为纯渠道）

```
extensions/pi-wechat-assistant/
├── index.ts              # 注册 WechatGateway 到 hub
└── src/
    ├── gateway.ts        # WechatGateway implements IGateway（薄，≈150 行）
    ├── client.ts         # iLink 协议收发（保留，行为不变）
    ├── api.ts media.ts auth.ts constants.ts utils.ts  # 协议辅助（原样保留）
    └── types.ts          # 渠道内部类型（不对外泄漏）
```

**删除**：`instances.ts`（协调逻辑上移）、`remote-commands.ts`（命令执行上移为 commands.ts 数据）、index.ts 内接管/桥/degraded 逻辑。

---

## 5. 数据流

### 5.1 微信消息 → agent → 回复

```
微信 → WechatGateway.getUpdates → normalize → hub.router
  ├─ 命令（/instances /use /msg /reload /model …含中文别名+语音容错）→ commands 表执行 → gateway.send
  ├─ 接管请求 → lock 仲裁 → 渠道启停切换
  └─ 普通消息 → queue.enqueue → bridge → pi.sendUserMessage
agent 回复 → hub 拦截 → router 查 lastChannel/lastUserId → gateway.send
```

### 5.2 接管 / 切换实例

```
/use 2（任意渠道）→ hub.commands → lock.take('wechat')
  → 成功：旧实例停轮询，新实例启轮询（渠道无感知）
  → 失败：锁被持有，提示当前持有者
```

命令在 TUI / 微信 / 飞书行为一致，与渠道无关。

---

## 6. 从 1f42593 重建的落地步骤

基准：`pi-wechat-assistant@1f42593`（support qa 版本，纯微信桥接，无协调逻辑，595 行 index.ts）。

| 步骤 | 内容 | 验收 |
|------|------|------|
| 1 | `git reset --hard 1f42593`，保留纯协议层（client/api/media/auth/message/queue 收发部分） | 微信单实例桥接行为不变 |
| 2 | 建 `pi-hub` 骨架：从 pi-coordinator 迁移 registry/lock/transport，合并 wechat/instances.ts 去重 | 实例列表/锁与现状等价 |
| 3 | 写 `WechatGateway`（IGateway 包装 WeixinClient），删 instances.ts / 协调桥 / degraded 逻辑 | 微信收发走 hub，行为对齐 |
| 4 | 命令表数据化：remote-commands.ts → hub/commands.ts（数据 + 通用匹配器） | /instances /use /msg /model 等全部可用 |
| 5 | Queue 加 ack/去重：修"读即删" | 模拟处理失败 → 消息重投不丢 |
| 6 | 全量验证：现有 tests（message/queue/questionnaire/utils）通过 + 双实例微信接管手动验证 | 回归通过 |
| 7 | （可选）写最小 feishu-gateway 验证扩展性 | 一个文件接入新 IM |

每一步独立可回退，行为以当前线上为准对齐。

---

## 7. 设计原则

1. **单向依赖**：gateway → hub，hub 通过接口调用 gateway，无循环
2. **一份状态源**：实例表/锁/队列全在 hub，渠道通过 API 查询，不直接读写文件
3. **at-least-once + 幂等**：消息不丢（ack 重投 + id 去重）
4. **命令与渠道解耦**：命令表是数据，任何渠道执行结果一致
5. **零新依赖起步**：JSON 文件 + HTTP，预留 SQLite/Redis/WS 实现位
