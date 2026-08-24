// ============================================================================
// 工具注册：list_instances / switch_instance / send_command / send_message
// / start_pi / dispatch_task
// 从 index.ts 抽出：纯注册逻辑，依赖通过 ToolsDeps 注入（闭包状态留在 index.ts）
// ============================================================================

import { Type } from '@sinclair/typebox'
// @ts-ignore — @earendil-works is the current package, but the older package still carries TS declarations used for compatibility here
import type { ExtensionAPI } from '@mariozechner/pi-coding-agent'
import type { InstanceInfo } from './types.js'
import type { TaskRegistry, SubTask } from './task.js'

export interface ToolsDeps {
  currentInstanceName: () => string
  collectInstances: () => Promise<{ local: InstanceInfo[]; all: InstanceInfo[] }>
  resolveTarget: (all: InstanceInfo[], name: string) => InstanceInfo | undefined
  doSwitch: (name: string, capability?: string) => Promise<string>
  doSendCommand: (target: string, command: string) => Promise<string>
  doSendMessage: (target: string, text: string) => Promise<string>
  doStartPi: (target: InstanceInfo, cwd?: string) => Promise<string>
  /** subagent 任务注册表 */
  taskRegistry: TaskRegistry
}

function ok(text: string) {
  return { content: [{ type: 'text' as const, text }], details: {} }
}

function fail(text: string) {
  return { content: [{ type: 'text' as const, text: `❌ ${text}` }], details: {} }
}

function fmtTask(t: SubTask): string {
  const age = Math.round((Date.now() - t.createdAt) / 1000)
  return `${t.id} [${t.status}] ${t.title} @${t.assignee} (${age}s${t.result ? ` | 结果:${t.result.slice(0, 40)}` : ''}${t.error ? ` | err:${t.error}` : ''})`
}

export function registerTools(pi: ExtensionAPI, deps: ToolsDeps): void {
  pi.registerTool({
    name: 'list_instances',
    label: 'List Instances',
    description: '列出所有 pi 实例（本机 + 远程），标注当前实例。供实例间协调、切换前查看。',
    promptSnippet: '列出所有 pi 实例',
    promptGuidelines: ['用户询问实例状态、切换前先调用本工具。编号可用于 switch_instance 的 target。'],
    parameters: Type.Object({}),
    async execute() {
      try {
        const { local, all } = await deps.collectInstances()
        if (all.length === 0) return ok('没有登记的实例')
        const lines = all.map((inst) => {
          const marks: string[] = []
          if (inst.pid === process.pid) marks.push('当前')
          if (!local.some((l) => l.name === inst.name)) marks.push('远程')
          const mark = marks.length > 0 ? `（${marks.join('，')}）` : ''
          return `${inst.name}${inst.host ? '@' + inst.host : ''}${mark}`
        })
        return ok(`实例列表：\n${lines.map((l, i) => `${i + 1}. ${l}`).join('\n')}`)
      } catch (err) {
        return fail(`列出实例失败: ${(err as Error).message}`)
      }
    },
  })

  pi.registerTool({
    name: 'switch_instance',
    label: 'Switch Instance',
    description: '把指定能力（默认接管）切换到目标实例，或在实例间发送接管请求。target 传实例名或编号。',
    promptSnippet: '切换实例接管',
    promptGuidelines: ['用户要求把控制权切到另一个实例时调用。先 list_instances 查看。'],
    parameters: Type.Object({
      target: Type.String({ description: '目标实例名或编号' }),
      capability: Type.Optional(Type.String({ description: '能力标识（如 wechat），未指定时默认 wechat（微信接管）' })),
    }),
    async execute(_toolCallId, params) {
      try {
        return ok(await deps.doSwitch(String(params.target).trim(), params.capability ?? 'wechat'))
      } catch (err) {
        return fail(`切换失败: ${(err as Error).message}`)
      }
    },
  })

  pi.registerTool({
    name: 'send_command',
    label: 'Send Command',
    description: '向指定 pi 实例发送斜杠指令（如 /new /reload /compact）或任意命令文本。',
    promptSnippet: '向实例发送指令',
    promptGuidelines: ['用户要求远程实例执行指令时调用。target 为目标实例名。'],
    parameters: Type.Object({
      target: Type.String({ description: '目标实例名' }),
      command: Type.String({ description: '要执行的指令，如 /reload' }),
    }),
    async execute(_toolCallId, params) {
      try {
        return ok(await deps.doSendCommand(String(params.target).trim(), String(params.command).trim()))
      } catch (err) {
        return fail(`发送指令失败: ${(err as Error).message}`)
      }
    },
  })

  pi.registerTool({
    name: 'send_message',
    label: 'Send Message',
    description: '向指定 pi 实例的用户/agent 发送普通消息（对方拉取后可见）。',
    promptSnippet: '向实例发送消息',
    promptGuidelines: ['用户要求给另一个实例/agent 留言时调用。'],
    parameters: Type.Object({
      target: Type.String({ description: '目标实例名' }),
      text: Type.String({ description: '消息内容' }),
    }),
    async execute(_toolCallId, params) {
      try {
        return ok(await deps.doSendMessage(String(params.target).trim(), String(params.text).trim()))
      } catch (err) {
        return fail(`发送消息失败: ${(err as Error).message}`)
      }
    },
  })

  pi.registerTool({
    name: 'start_pi',
    label: 'Start Pi',
    description: '在指定实例（默认当前本机）的 tmux pi 会话中启动 pi。',
    promptSnippet: '在实例上启动 pi',
    promptGuidelines: ['用户要求启动 pi / start pi / 启动派时调用。'],
    parameters: Type.Object({
      target: Type.Optional(Type.String({ description: '目标实例名（默认当前本机）' })),
      cwd: Type.Optional(Type.String({ description: '启动目录（默认实例 cwd 或 ~）' })),
    }),
    async execute(_toolCallId, params) {
      try {
        const { all } = await deps.collectInstances()
        let inst: InstanceInfo | undefined
        if (params.target) {
          inst = deps.resolveTarget(all, String(params.target).trim())
          if (!inst) return fail(`未找到实例 ${params.target}，先 /instances 查看`)
        } else {
          inst = all.find((i) => i.name === deps.currentInstanceName())
          if (!inst) return fail('未找到当前实例')
        }
        return ok(await deps.doStartPi(inst, params.cwd ? String(params.cwd) : undefined))
      } catch (err) {
        return fail(`启动失败: ${(err as Error).message}`)
      }
    },
  })

  pi.registerTool({
    name: 'dispatch_task',
    label: 'Dispatch Task',
    description: '向一个或多个子实例分发子任务（写任务注册表 + 带 TASK#N 标记的消息），等待各实例回传 [TASK#N结果] 后汇总。',
    promptSnippet: '分发子任务给多个实例',
    promptGuidelines: ['subagent 协作：先 list_instances 确认可用实例，再分发任务；结果由子实例 send_message 回传。用 /tasks 或 task_list 跟踪状态。'],
    parameters: Type.Object({
      tasks: Type.Array(Type.Object({
        instance: Type.String({ description: '目标实例名' }),
        task: Type.String({ description: '任务描述，建议明确输出格式（JSON/表格/固定模板）' }),
      })),
    }),
    async execute(_toolCallId, params) {
      try {
        const lines: string[] = []
        const ids: string[] = []
        for (const [i, t] of params.tasks.entries()) {
          const tag = `[TASK#${i + 1}]`
          // 写任务注册表（状态跟踪/超时/自动回收）
          const task = deps.taskRegistry.create({
            title: t.task.slice(0, 40),
            assignee: t.instance,
            payload: t.task,
          })
          ids.push(task.id)
          const reply = await deps.doSendMessage(t.instance, `${tag} ${t.task}\n完成后请回复「${tag}结果」+ 内容（建议 JSON），任务ID: ${task.id}`)
          lines.push(`${tag} → ${t.instance}: ${reply}（${task.id}）`)
        }
        return ok(`已分发 ${params.tasks.length} 个任务：\n${lines.join('\n')}\n\n等待各实例回传「[TASK#N结果]」，收到后请汇总。任务列表可用 /tasks 或 task_list 查看。`)
      } catch (err) {
        return fail(`分发失败: ${(err as Error).message}`)
      }
    },
  })

  pi.registerTool({
    name: 'task_list',
    label: 'List Tasks',
    description: '列出所有 subagent 任务（按状态过滤可选：pending/running/done/failed/timeout）。',
    promptSnippet: '列出 subagent 任务',
    promptGuidelines: ['用户问任务进度/有哪些任务时调用。'],
    parameters: Type.Object({
      status: Type.Optional(Type.String({ description: '按状态过滤（pending/running/done/failed/timeout）' })),
    }),
    async execute(_toolCallId, params) {
      try {
        const all = deps.taskRegistry.list()
        const filtered = params.status ? all.filter((t) => t.status === params.status) : all
        if (filtered.length === 0) return ok('没有任务')
        return ok(`任务列表（${filtered.length}/${all.length}）：\n${filtered.map(fmtTask).join('\n')}`)
      } catch (err) {
        return fail(`查询任务失败: ${(err as Error).message}`)
      }
    },
  })

  pi.registerTool({
    name: 'task_status',
    label: 'Task Status',
    description: '查询单个 subagent 任务详情（状态/结果/错误/耗时）。',
    promptSnippet: '查询任务状态',
    promptGuidelines: ['用户问某个任务的状态时调用。'],
    parameters: Type.Object({
      id: Type.String({ description: '任务 ID（如 TASK-1787...）' }),
    }),
    async execute(_toolCallId, params) {
      try {
        const t = deps.taskRegistry.get(String(params.id).trim())
        if (!t) return fail(`未找到任务 ${params.id}`)
        return ok(`任务详情：\n${fmtTask(t)}\n\npayload: ${t.payload}\n结果: ${t.result ?? '(无)'}\n错误: ${t.error ?? '(无)'}`)
      } catch (err) {
        return fail(`查询失败: ${(err as Error).message}`)
      }
    },
  })

  pi.registerTool({
    name: 'task_reclaim',
    label: 'Reclaim Subagent',
    description: '手动回收子实例（发 /quit + 标记其任务），防泄漏。目标可传实例名或任务 ID。',
    promptSnippet: '回收子代理',
    promptGuidelines: ['用户要求清理/回收子代理时调用；任务全部完成后的自动回收由 taskMonitor 处理。'],
    parameters: Type.Object({
      target: Type.String({ description: '实例名或任务 ID' }),
    }),
    async execute(_toolCallId, params) {
      try {
        const target = String(params.target).trim()
        const tasks = deps.taskRegistry.list()
        // 任务 ID → 其 assignee
        const byId = tasks.find((t) => t.id === target)
        const assignee = byId?.assignee ?? target
        const owned = tasks.filter((t) => t.assignee === assignee)
        if (owned.length > 0) {
          for (const t of owned) deps.taskRegistry.update(t.id, { status: t.status === 'pending' || t.status === 'running' ? 'failed' : t.status, error: (t.error ?? '') + ' 手动回收' })
        }
        const reply = await deps.doSendCommand(assignee, '/quit')
        return ok(`已回收 ${assignee}：${reply}${owned.length > 0 ? `（${owned.length} 个任务已标记）` : ''}`)
      } catch (err) {
        return fail(`回收失败: ${(err as Error).message}`)
      }
    },
  })

  pi.registerTool({
    name: 'task_retry',
    label: 'Retry Task',
    description: '重试失败/超时的 subagent 任务（重新分发）。',
    promptSnippet: '重试任务',
    promptGuidelines: ['任务 failed/timeout 且用户要求重试时调用。'],
    parameters: Type.Object({
      id: Type.String({ description: '任务 ID' }),
    }),
    async execute(_toolCallId, params) {
      try {
        const id = String(params.id).trim()
        const t = deps.taskRegistry.get(id)
        if (!t) return fail(`未找到任务 ${id}`)
        deps.taskRegistry.update(id, { status: 'pending', attempts: t.attempts + 1, deadline: Date.now() + 10 * 60_000, error: '手动重试' })
        const tag = `[TASK#${id.slice(-3)}]`
        const reply = await deps.doSendMessage(t.assignee, `${tag} ${t.payload}\n（重试）完成后请回复「${tag}结果」+ 内容，任务ID: ${t.id}`)
        return ok(`已重试 ${id} → ${t.assignee}: ${reply}`)
      } catch (err) {
        return fail(`重试失败: ${(err as Error).message}`)
      }
    },
  })
}
