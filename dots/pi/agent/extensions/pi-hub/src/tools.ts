// ============================================================================
// 工具注册：list_instances / switch_instance / send_command / send_message
// / start_pi / dispatch_task
// 从 index.ts 抽出：纯注册逻辑，依赖通过 ToolsDeps 注入（闭包状态留在 index.ts）
// ============================================================================

import { Type } from '@sinclair/typebox'
// @ts-ignore — @earendil-works is the current package, but the older package still carries TS declarations used for compatibility here
import type { ExtensionAPI } from '@mariozechner/pi-coding-agent'
import type { InstanceInfo } from './types.js'

export interface ToolsDeps {
  currentInstanceName: () => string
  collectInstances: () => Promise<{ local: InstanceInfo[]; all: InstanceInfo[] }>
  resolveTarget: (all: InstanceInfo[], name: string) => InstanceInfo | undefined
  doSwitch: (name: string, capability?: string) => Promise<string>
  doSendCommand: (target: string, command: string) => Promise<string>
  doSendMessage: (target: string, text: string) => Promise<string>
  doStartPi: (target: InstanceInfo, cwd?: string) => Promise<string>
}

function ok(text: string) {
  return { content: [{ type: 'text' as const, text }], details: {} }
}

function fail(text: string) {
  return { content: [{ type: 'text' as const, text: `❌ ${text}` }], details: {} }
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
    description: '向一个或多个子实例分发子任务（带 TASK#N 标记的消息），等待各实例回传 [TASK#N结果] 后汇总。',
    promptSnippet: '分发子任务给多个实例',
    promptGuidelines: ['subagent 协作：先 list_instances 确认可用实例，再分发任务；结果由子实例 send_message 回传。'],
    parameters: Type.Object({
      tasks: Type.Array(Type.Object({
        instance: Type.String({ description: '目标实例名' }),
        task: Type.String({ description: '任务描述，建议明确输出格式（JSON/表格/固定模板）' }),
      })),
    }),
    async execute(_toolCallId, params) {
      try {
        const lines: string[] = []
        for (const [i, t] of params.tasks.entries()) {
          const tag = `[TASK#${i + 1}]`
          const reply = await deps.doSendMessage(t.instance, `${tag} ${t.task}\n完成后请回复「${tag}结果」+ 结果内容。`)
          lines.push(`TASK#${i + 1} → ${t.instance}: ${reply}`)
        }
        return ok(`已分发 ${params.tasks.length} 个任务：\n${lines.join('\n')}\n\n等待各实例回传「[TASK#N结果]」，收到后请汇总。`)
      } catch (err) {
        return fail(`分发失败: ${(err as Error).message}`)
      }
    },
  })
}
