# Subagent 任务管理 — 实施方案

> 目标：在 pi-hub 现有 envelope 通道之上，建立**任务注册表 + 状态跟踪 + 超时监控 + 自动回收**，
> 解决当前 `dispatch_task` 的痛点：无状态跟踪、无超时、子实例泄漏、结果不可靠。

## 一、现状与痛点

当前流程：`dispatch_task` 发 `[TASK#N]` 消息 → 子实例回传 `[TASK#N结果]` → 主实例汇总 → 手动 `/cmd /quit` 回收。

| 痛点 | 后果 |
|------|------|
| 无任务状态跟踪 | 主实例不知道子任务进行到哪 |
| 无超时机制 | 子实例卡死 → 任务悬挂 |
| 子实例手动回收 | 忘 `/quit` → 泄漏 |
| 结果靠自由文本回传 | 格式乱 → 汇总难 |
| 无持久化 | 主实例崩溃 → 任务丢失 |

## 二、设计

### 2.1 数据模型（`src/task.ts`，持久化 `tasks.json`）

```ts
interface SubTask {
  id: string          // TASK-<ts>-<rand>
  title: string       // 人类可读描述
  assignee: string    // 子实例名
  status: 'pending' | 'running' | 'done' | 'failed' | 'timeout'
  payload: string     // 任务描述（分发时发给子实例的正文）
  createdAt: number
  deadline: number    // createdAt + TASK_TTL_MS（默认 10min，可配）
  result?: string     // 回传结果（JSON 或文本）
  error?: string
  attempts: number    // 重试次数（上限 TASK_MAX_ATTEMPTS=2）
  keep?: boolean      // 完成后不自动回收（调试用）
}
```

**TaskRegistry** 类：
- `create(payload, assignee)` → 写注册表 + 返回 id
- `update(id, patch)` → 更新状态/结果
- `get(id)` / `list()`
- `monitor(now)` → 返回需要处理的事件（超时任务 / 可回收实例）
- 持久化：`tasks.json`（STATE_DIR），**tmp+rename 原子写**（同 registry.ts 模式）

### 2.2 结构化回传协议

子实例完成时回传（保持 `[TASK#N结果]` 前缀兼容现有流程，正文建议 JSON）：

```
[TASK#1结果] {"status":"done","data":{...}}
[TASK#1结果] {"status":"failed","error":"原因"}
```

主实例 agent 在汇总时解析；注册表只负责**记录回传原文**（`result` 字段），不强制自动解析（第一步不做消息拦截）。

### 2.3 生命周期自动化（`taskMonitor`，主实例 30s 轮询）

| 事件 | 动作 |
|------|------|
| `pending/running` 超 `deadline` | 标记 `timeout`，发催办消息「[TASK#N] 超时，请尽快回传或说明阻塞」 |
| `timeout` 且 `attempts < MAX` | 重新分发（attempts+1，重置 deadline）→ `running` |
| `timeout` 且 attempts 达上限 | 保持 `timeout`（等待人工 `task_retry`） |
| 实例的**全部任务** `done/failed/timeout` | **自动 `/cmd <实例> /quit` 回收**（防泄漏） |
| 主实例启动 | 读 `tasks.json` 恢复：`running` 且已超时 → `timeout` |

**回收保护**：
- 不回收协调中心实例（`coordinatorPort` 模式）——`/quit` 会中断全部客户端
- `keep: true` 的任务对应实例不自动回收
- 子实例可能承担多个任务 → 全部结束才回收

### 2.4 工具与命令

**工具增强**（`src/tools.ts`）：
- `dispatch_task`（增强）：每个任务写注册表 + 发消息，返回任务 id 列表；任务消息附带 deadline 提示
- `task_status <id>`：查单个任务（状态/结果/耗时）
- `task_list`：列出所有任务（按状态过滤）
- `task_reclaim <实例|id>`：手动回收（发 `/quit` + 标记）
- `task_retry <id>`：重试失败/超时任务（重新分发）

**命令**（`src/commands.ts` 命令表，TUI 与微信共用）：
- `/tasks`：查看所有任务（状态/负责人/结果摘要）
- `/task <id>`：任务详情

### 2.5 边界与约束

- **不改传输层**：任务消息仍走 `message` envelope（`[TASK#N]` 标记），兼容现有子实例
- **主实例职责**：只有发起方（主实例）维护自己的 `tasks.json`；子实例不感知任务框架（只按消息约定回传）
- **多主实例**：每个实例各自维护 `tasks.json`（互不干扰）
- **失败隔离**：单任务失败不影响其他任务

## 三、实施步骤（依次）

| 步骤 | 内容 | 文件 |
|------|------|------|
| **1** | TaskRegistry + `tasks.json` 持久化 + monitor | 新增 `src/task.ts` |
| **2** | `dispatch_task` 增强（写注册表）；新增 `task_status` / `task_list` / `task_reclaim` / `task_retry` 工具 | `src/tools.ts` |
| **3** | `/tasks` `/task` 命令（命令表） | `src/commands.ts` + `index.ts` CommandCtx |
| **4** | `taskMonitor` 轮询（30s：超时催办 / 自动回收） | `index.ts` |
| **5** | 单元测试（注册表 CRUD / monitor 超时与回收判定）+ 实测 | `tests/task.test.ts` |

**每步验证**：`npx tsc --noEmit` + `npx vitest run`（60 用例不得回归）+ 行为自检。

## 四、验证方法

1. 单测：TaskRegistry CRUD、monitor 超时标记、回收判定（实例全部 done → 回收）
2. 实测：`start_pi` 起子实例 → `dispatch_task` 2 个任务 → 观察 `/tasks` 状态流转（pending→running→done）→ 子实例回传后自动 `/quit` 回收
3. 回归：现有 60 用例 + 微信接管/切换不受影响

## 五、后续增强（非本次范围）

- **结构化回传自动解析**：hook 消息流，`[TASK#N结果]` 自动更新注册表（无需主实例 agent 介入）
- **依赖编排**：`dependsOn` 字段，任务 B 等 A done 才分发
- **资源限制**：并发子实例上限
- **结果汇总模板**：多任务结果自动合并为表格/报告
