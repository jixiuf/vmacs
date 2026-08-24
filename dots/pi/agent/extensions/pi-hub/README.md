# pi-hub

pi session 间协调核心扩展：实例注册、接管切换、指令/消息互发、广播重载、远程命令执行。
IM 渠道（wechat 等）通过 `IGateway` 契约接入，渠道只依赖类型不依赖协调实现。

## 功能特性

- **实例注册表**：本机/远程实例登记、存活检测、跨机器发现（`instances.json`，原子写，多实例并发安全）
- **接管切换**：微信等能力在实例间一键切换（`/use`），带全局锁仲裁
- **指令/消息互发**：`/cmd` 向实例发指令（真正执行），`/msg` 发消息
- **广播重载**：`/reloadall` 一键重载所有实例（当前实例本地执行，其他实例注入触发）
- **远程命令执行**：`/cmd <实例> /new`、`/fork`、`/goto`、`/reload`、`/name` 等真正在目标实例执行
- **启动 pi**：`/start-pi` 在指定实例（默认本机）的 tmux 会话中开窗口启动 pi
- **宽松 use 匹配**：微信里说「2」「use home」「home」即可切换接管（语音友好）
- **IM 渠道接入**：任何渠道实现 `IGateway` 即接入（当前有 wechat）

## 架构

```
┌─────────────────────────────────────────────────────┐
│  IM 渠道 (WechatGateway 等)                          │
│  └── InboundMessage ──► Router（命令分类）           │
│        ├─ 协调命令 (/instances /use /cmd /msg …)      │
│        └─ 渠道自有处理（会话命令 / 问卷 / 入队）        │
├─────────────────────────────────────────────────────┤
│  Envelope 传输协议（跨实例统一）                      │
│  message / command / takeover / lock / broadcast     │
│  ├─ 协调中心模式：本机 HTTP+WS 服务（8089）+ 本地队列 │
│  └─ 客户端模式：WS 长连接（双向推送，即时投递）       │
├─────────────────────────────────────────────────────┤
│  实例注册表 registry（instances.json）               │
│  全局锁 lock / 队列 queue / 广播 broadcast           │
└─────────────────────────────────────────────────────┘
```

### 通信：WebSocket 双向（替代 HTTP 短轮询）

- 协调中心与客户端间为 **WS 长连接**（`/ws?name=&host=`），服务器**即时推送** envelope（延迟毫秒级，无 2s 轮询）
- WS 为**手写帧实现（无 npm 依赖）**：握手（SHA1 accept）+ 帧编解码（mask/len）
- **服务器主动心跳**：每 15s 发协议层 PING，客户端（标准 WebSocket）自动回 PONG → 刷新活跃登记（`ACTIVE_CLIENT_TTL=30s`）。进程活着但心跳停止的实例不再被误展示（修复文本帧心跳不被识别的问题）
- 客户端断线**指数退避重连**（1s→15s）；投递时目标不在线则入队，连接后补投（**投递成功即 ack**，防重启/重连重复投递）
- 协调中心故障转移：协调中心退出后，其他配置了 `coordinatorPort` 的实例自动接管（`ensureCoordinatorIfNeeded`）
- 保留 HTTP 查询端点（`/instances` `/lock` `/lastmsg`）与 `/envelope` POST（发送 fallback）

### 关键设计

- **注入即可执行**：`sendUserMessage(..., { expandPromptTemplates: true })` 注入的斜杠命令会被 **pi 命令系统 dispatch**（非仅显示）。因此 `send_command /__hub_reload`（或 `/__hub_cmd /reload`）可**可靠触发远程 reload**（实测验证）；TUI 手动 `/reload` 仍是兜底。
- **循环防护**：指令发给自己时本地处理（`isCurrentInstance`），不再经协调中心回投注入，避免“发送→注入→再发送”死循环。
- **stale ctx 防护**：`ctx.reload()` 后命令上下文立即失效，reload 放最后执行。
- **注册表并发安全**：`listInstances` 只读不写回（防并发读-改-写清空注册表），写入走 tmp+rename 原子写。
- **全局锁**：`coordinator-lock.json` 仲裁多实例接管。客户端 3s 续约（原 1s，降频降 IO），`TTL=15s` 余量充足；`force` 可立即抢占；损坏/无主锁文件自动清理（防 wx 原子创建死锁）。
- **日志**：`/tmp/pi-hub.log`（`logEvent` 统一落盘，5MB 自动轮转）；调试日志 `PI_COORDINATOR_DEBUG=1` → console。

## 代码结构

```
index.ts          入口/装配（生命周期、轮询、桥、公共逻辑）
src/commands.ts   命令表（TUI 与渠道共用一份实现）
src/tools.ts      工具注册（list_instances / switch_instance / …）
src/transport.ts  协调中心 HTTP+WS / 客户端 WS / SSH 通道
src/router.ts     入站路由 + envelope 统一分发
src/queue.ts      可靠消息队列（ack + dedup + TTL 2min + 重投上限 3）
src/lock.ts       全局锁（TTL 15s / force / 损坏自愈）
src/registry.ts   实例注册表（原子写）
src/start-pi.ts   tmux 启动 pi（纯逻辑 + 依赖注入）
src/logger.ts     统一日志（debug 开关 + 落盘轮转）
src/config.ts     配置读取（双文件回退 + 自动重建）
```

## 安装

扩展目录安装（与 pi 扩展机制一致）：

```bash
# 将本目录放入 pi 扩展加载路径（如 ~/.pi/agent/extensions/pi-hub）
# 在 pi 中 /reload 加载，/instances 验证
```

依赖：`@earendil-works/pi-coding-agent` 或 `@mariozechner/pi-coding-agent`（peer）。

## 配置

配置位于 `~/.pi/agent/coordinator/config.json`（优先），缺失字段回退 `~/.pi/agent/wechat-assistant/config.json`：

```json
{
  "instanceName": "home",
  "coordinatorPort": 8089,
  "coordinatorUrl": "http://10.170.16.16:8089",
  "remoteInstanceNames": ["ljmacjxf"],
  "remoteHosts": {
    "pigw": { "target": "bj-vc-client-apm-01", "port": 22 }
  },
  "channels": {
    "wechat": { "enabled": true, "autoTakeover": true }
  }
}
```

| 字段 | 说明 |
|------|------|
| `instanceName` | 当前实例名（缺省取 cwd basename） |
| `coordinatorPort` | 协调中心模式：本机监听端口（作为仲裁者） |
| `coordinatorUrl` | 客户端模式：协调中心地址（轮询 inbox） |
| `remoteInstanceNames` | 已知但可能离线的远程实例名 |
| `remoteHosts` | 远程主机 SSH 配置（实例名 → { target, port }） |
| `channels` | 渠道开关（wechat 等） |

> 协调中心（server）与客户端只能二选一：设 `coordinatorPort` 为本机仲裁者；设 `coordinatorUrl` 为接入远程协调中心。

## 命令

### 斜杠命令（TUI）

| 命令 | 说明 |
|------|------|
| `/instances` | 列出所有实例（本机+远程，标注当前） |
| `/use <实例>` | 切换微信接管（实例名/编号，默认复用上次） |
| `/cmd [实例] <指令>` | 向实例发送指令（远程真正执行） |
| `/msg [实例] <内容>` | 向实例发送消息 |
| `/reloadall` | 重载所有实例（当前本地执行，其他注入触发） |
| `/start-pi [实例] [目录]` | 在实例 tmux 会话中启动 pi（默认本机） |

### 微信命令（中文别名 / 语音友好）

| 说法 | 等价 |
|------|------|
| 「实例列表」「所有实例」 | `/instances` |
| 「切换」「切到 X」/ 说「2」「use home」「home」 | `/use` |
| 「发送命令」「执行命令」 | `/cmd` |
| 「发送消息」「发消息」 | `/msg` |
| 「重载全部」「全部重载」「reload all」 | `/reloadall` |
| 「启动pi」「启动派」「start pi」「开pi」 | `/start-pi` |

### Agent 工具

| 工具 | 说明 |
|------|------|
| `list_instances` | 列出所有 pi 实例 |
| `switch_instance` | 切换接管（capability 默认 wechat） |
| `send_command` | 向实例发送指令 |
| `send_message` | 向实例发送消息 |
| `start_pi` | 在实例 tmux 中启动 pi |
| `dispatch_task` | 向多个子实例分发子任务（带 TASK#N 标记） |

## Subagents：任务分发与汇总

**原则：subagent = 按需创建的新 pi session，用完即回收（/quit）**。
不要复用存活节点（如协调中心 home）当 subagent——存活节点承担协调/微信职责，不应被任务占用或退出。

```
主实例 agent
  ├─ start_pi home 或其他机器      → 起全新 pi 会话（subagent，独立 tmux 窗口/会话）
  ├─ dispatch_task([{instance: subagent, task: "分析..."}])
  │     → 自动带 [TASK#N] 标记发消息给子实例
  ▼
子实例 agent 处理任务 → 回传 [TASK#N结果]
  ▼
主实例 agent 按 ID 汇总
  ▼
/cmd <subagent> /quit → 用完即回收（实例自动注销）
```

- **创建**：`/start-pi <主机>` 起专用 subagent（窗口名 `pi-<实例名>-<pid>`，唯一）；或 `start_pi` 工具
- **分发**：`dispatch_task` 自动加 `[TASK#N]`，要求子实例回传 `[TASK#N结果]` + 固定格式（JSON/表格）
- **催办**：无回传时 `/msg <subagent> 请回复 [TASK#N] 进度`
- **回收**：`/cmd <subagent> /quit` 退出（**不要对协调中心实例用 /quit**，会中断全部客户端）
- **协调中心**：保持一个稳定节点（配置 `coordinatorPort`）；subagent 用完即弃，不承担协调职责

## 远程命令执行

`/cmd <实例> /xxx` 的执行链路（**envelope 注入，不依赖 tmux 模拟输入**）：

- 指令包装为 `/__hub_cmd <指令>`，经协调中心投递目标实例 → pi 命令系统分发 `__hub_cmd` handler 真正执行
- 目标实例必须运行新版 pi-hub（旧版不认识 `__hub_cmd`，指令会被忽略）

`__hub_cmd` 支持：

| 指令 | 执行 |
|------|------|
| `/new` | `ctx.newSession()` |
| `/fork <id>` | `ctx.fork(id)` |
| `/goto <path>` / `/switch <path>` | `ctx.switchSession(path)` |
| `/reload` | `ctx.reload()` |
| `/name <名称>` | `pi.setSessionName(name)` |
| `/thinking <level>` | `pi.setThinkingLevel(level)` |
| `/quit` | `process.exit(0)` 退出进程（不依赖 TUI；注册条目由定时 prune 清理） |

> 当前实例自己的指令仅本地处理 `/reload`（session 命令需要 TUI 上下文）。
> 未知指令仅记录忽略（不做不可靠的 tmux 模拟输入）。

## start-pi

在目标实例（默认本机）的 **tmux 会话中开窗口**启动 pi（不新建会话）：

- 定位实例所在 tmux 会话：本机读 `process.env.TMUX`，远程读 `/proc/<pid>/environ` 的 TMUX 变量映射会话
- 窗口名唯一：`pi-<实例名>-<pid>`（pid 未知回退时间戳），已存在则去重
- 窗口目标用 **session id**（`$N` 形式），避免纯数字会话名被当作窗口索引（"index 0 in use" 坑）
- 安全：实例名白名单 + 目录拒绝 shell 元字符（防注入，远程=shell 执行必须校验）

## Envelope 协议

跨实例统一传输（`types.ts`）：

```ts
type Envelope =
  | { type: 'message';   from; to; text; ts }
  | { type: 'command';   from; to; command; ts }
  | { type: 'takeover';  from; to; capability; ts }
  | { type: 'lock';      from; capability; ts }
  | { type: 'broadcast'; from; command; ts }
```

- **协调中心模式**（server）：本机 `queue.enqueue`，WS 客户端即时推送 / HTTP `/envelope` 接收投递；**协调中心自己也消费发给自己的队列**（2s 轮询，ack 防重投）
- **客户端模式**：WS 长连接订阅（`connectCoordinatorWS`），服务器推送 envelope → `routeEnvelope` 分发；发送经 WS，断线回退 HTTP POST
- 目标命名：实例名（`name@host` 由 `resolveTarget` 归一化，解决全名投递不匹配问题）

## 开发

```bash
# 类型检查（项目无脚本，直接用 tsc）
npx tsc --noEmit
# 测试
npx vitest run
```

测试：60 个用例（中文数字解析 / 消息队列 ack / 全局锁 / 命令归一化 / 宽松 use / start-pi / reloadall / 心跳 / 去重）。

## License

MIT
