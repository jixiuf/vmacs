# Pi 扩展能力清单

> 自动生成于 2026-08-23T09:55:19.209Z（scripts/export-docs.mjs）
> 覆盖扩展：pi-hub（实例协调）、pi-wechat-assistant（微信桥）

## pi-hub — Agent 工具（6）

| 工具 | 说明 | 触发提示 | 使用准则 |
|------|------|---------|---------|
| `list_instances` | 列出所有 pi 实例（本机 + 远程），标注当前实例。供实例间协调、切换前查看。 | 列出所有 pi 实例 | 用户询问实例状态、切换前先调用本工具。编号可用于 switch_instance 的 target。 |
| `switch_instance` | 把指定能力（默认接管）切换到目标实例，或在实例间发送接管请求。target 传实例名或编号。 | 切换实例接管 | 用户要求把控制权切到另一个实例时调用。先 list_instances 查看。 |
| `send_command` | 向指定 pi 实例发送斜杠指令（如 /new /reload /compact）或任意命令文本。 | 向实例发送指令 | 用户要求远程实例执行指令时调用。target 为目标实例名。 |
| `send_message` | 向指定 pi 实例的用户/agent 发送普通消息（对方拉取后可见）。 | 向实例发送消息 | 用户要求给另一个实例/agent 留言时调用。 |
| `start_pi` | 在指定实例（默认当前本机）的 tmux pi 会话中启动 pi。 | 在实例上启动 pi | 用户要求启动 pi / start pi / 启动派时调用。 |
| `dispatch_task` | 向一个或多个子实例分发子任务（带 TASK#N 标记的消息），等待各实例回传 [TASK#N结果] 后汇总。 | 分发子任务给多个实例 | subagent 协作：先 list_instances 确认可用实例，再分发任务；结果由子实例 send_message 回传。 |

## pi-hub — TUI 斜杠命令（10）

| 命令 | 说明 |
|------|------|
| `/instances` | 列出所有 pi 实例（本机 + 远程） |
| `/use` | 把接管权切换到指定实例（实例名/编号/默认复用上次） |
| `/send-command` | 向实例发送指令：/send-command [实例名] <指令>（不写实例名复用上次） |
| `/send-message` | 向实例发送消息：/send-message [实例名] <内容>（不写实例名复用上次） |
| `/cmd` | 别名：/send-command |
| `/msg` | 别名：/send-message |
| `/__hub_reload` | 广播重载（内部命令，由 broadcast envelope 触发） |
| `/__hub_cmd` | 内部命令：远程执行会话命令（/new /fork /goto /reload /name 等，未知命令回退 tmux 模拟输入） |
| `/reloadall` | 重载所有实例（当前实例本地执行，其他实例发指令） |
| `/start-pi` | 在实例（默认本机）的 tmux pi 会话中启动 pi：/start-pi [实例名] [目录] |

## pi-hub — 渠道命令表（微信/语音别名，6）

| 命令 | 中文别名 / 语音说法 |
|------|---------------------|
| `instances` | 实例列表、实力列表、所有实例、全部实例、正在运行的实例、实例、实例列、学历列表、学力列表 |
| `use` | 切换、切换到、切到 |
| `cmd` | 发送命令、执行命令 |
| `msg` | 发送消息、发消息 |
| `start-pi` | start pi、start-pi、启动pi、启动派、启动皮、启动Pi、启动一个pi、开pi、开个pi |
| `reloadall` | 重载全部、全部重载、reload all、reloadall、重载所有、重启全部 |

## pi-wechat-assistant — 微信远程命令（17）

| 命令 |
|------|
| `/new` |
| `/prev` |
| `/next` |
| `/reloadAll` |
| `/reload` |
| `/sessions` |
| `/goto` |
| `/model` |
| `/thinking` |
| `/tools` |
| `/compact` |
| `/stop` |
| `/status` |
| `/config` |
| `/name` |
| `/session` |
| `/help` |

## 架构速览

- **pi-hub**：协调中心（8089）+ WS 双向通信；实例注册/发现、接管切换、指令/消息互发、reloadall、start-pi、subagent 分发汇总
- **pi-wechat-assistant**：WechatGateway（iLink 轮询）→ hub（协调命令）→ 会话；消息/图片/文件、会话管理、远程命令

## 相关文档

- pi-hub: `pi-hub/README.md`（架构、配置、命令）
- pi-wechat-assistant: `pi-wechat-assistant/README.md`
- Skills: `~/.agents/skills/pi-hub/SKILL.md`、`~/.agents/skills/pi-wechat-assistant/SKILL.md`
