// ============================================================================
// 命令表：斜杠命令 + 中文别名 + 语音容错（纯数据 + 通用匹配器，渠道无关）
// 从 wechat remote-commands.ts 数据化上移；/instances /use /msg /cmd 等在 hub 实现一次
// ============================================================================

import type { InstanceInfo } from './types.js'

export interface CommandCtx {
  /** 当前实例名 */
  currentInstanceName: string
  /** 收集所有实例（本机 + 远程） */
  collectInstances: () => Promise<{ local: InstanceInfo[]; all: InstanceInfo[] }>
  /** 实例名/编号 → InstanceInfo */
  resolveTarget: (all: InstanceInfo[], name: string) => InstanceInfo | undefined
  /** 切换接管 */
  doSwitch: (name: string, capability?: string) => Promise<string>
  /** 发送指令到实例 */
  doSendCommand: (target: string, command: string) => Promise<string>
  /** 发送消息到实例 */
  doSendMessage: (target: string, text: string) => Promise<string>
  /** 记住上次目标实例 */
  rememberTarget: (name: string) => void
  /** 获取上次目标实例 */
  getLastTarget: () => string | null
}

export interface CommandResult {
  reply: string | null
  consumed: boolean
}

type CommandFn = (args: string, ctx: CommandCtx) => Promise<CommandResult>

// ============================================================================
// 中文数字解析（兼容语音转文字）
// ============================================================================

export function parseChineseNumber(s: string): number | null {
  const map: Record<string, number> = {
    一: 1, 壹: 1, 衣: 1, 医: 1,
    两: 2, 二: 2, 贰: 2, 耳: 2,
    三: 3, 叁: 3,
    四: 4, 肆: 4, 寺: 4,
    五: 5, 伍: 5, 午: 5, 屋: 5, 无: 5, 吾: 5,
    六: 6, 陆: 6,
    七: 7, 柒: 7,
    八: 8, 捌: 8,
    九: 9, 玖: 9,
    十: 10, 拾: 10,
  }
  if (s in map) return map[s]
  const m1 = s.match(/^十([一二三四五六七八九])?$/)
  if (m1) return 10 + (m1[1] ? map[m1[1]] : 0)
  const m2 = s.match(/^二十([一二三四五六七八九])?$/)
  if (m2) return 20 + (m2[1] ? map[m2[1]] : 0)
  return null
}

export function toNumber(s: string): number {
  return parseChineseNumber(s) ?? parseInt(s, 10)
}

// ============================================================================
// 命令实现
// ============================================================================

async function cmdInstances(_args: string, ctx: CommandCtx): Promise<CommandResult> {
  const { local, all } = await ctx.collectInstances()
  if (all.length === 0) return { reply: '没有登记的实例', consumed: true }
  const lines = all.map((inst) => {
    const marks: string[] = []
    if (inst.pid === process.pid) marks.push('当前')
    if (!local.some((l) => l.name === inst.name)) marks.push('远程')
    const mark = marks.length > 0 ? `（${marks.join('，')}）` : ''
    return `${inst.name}${inst.host ? '@' + inst.host : ''}${mark}`
  })
  return { reply: `实例列表：\n${lines.map((l, i) => `${i + 1}. ${l}`).join('\n')}`, consumed: true }
}

async function cmdUse(args: string, ctx: CommandCtx): Promise<CommandResult> {
  const name = args.trim()
  if (!name) {
    const last = ctx.getLastTarget()
    if (last) return { reply: await ctx.doSwitch(last, 'wechat'), consumed: true }
    return { reply: '用法: /use <实例名或编号>，如 /use pigw 或 /use 2', consumed: true }
  }
  return { reply: await ctx.doSwitch(name, 'wechat'), consumed: true }
}

async function cmdSendCommand(args: string, ctx: CommandCtx): Promise<CommandResult> {
  const { target, rest } = await parseTargetArgs(args, ctx)
  if (!rest) return { reply: '用法: /cmd [实例名] <指令>，如 /cmd /reload 或 /cmd home /reload', consumed: true }
  if (target) return { reply: await ctx.doSendCommand(target, rest), consumed: true }
  const last = ctx.getLastTarget()
  if (last) return { reply: await ctx.doSendCommand(last, rest), consumed: true }
  return { reply: '未指定实例且无上次实例，先 /use <实例>', consumed: true }
}

async function cmdSendMessage(args: string, ctx: CommandCtx): Promise<CommandResult> {
  const { target, rest } = await parseTargetArgs(args, ctx)
  if (!rest) return { reply: '用法: /msg [实例名] <内容>，如 /msg hello 或 /msg home hello', consumed: true }
  if (target) return { reply: await ctx.doSendMessage(target, rest), consumed: true }
  const last = ctx.getLastTarget()
  if (last) return { reply: await ctx.doSendMessage(last, rest), consumed: true }
  return { reply: '未指定实例且无上次实例，先 /use <实例>', consumed: true }
}

/** /cmd /msg 解析：参数 1 若匹配已知实例名则视为实例，否则复用上次实例并把全部参数当内容 */
async function parseTargetArgs(args: string, ctx: CommandCtx): Promise<{ target: string | null; rest: string }> {
  const parts = args.trim().split(/\s+/)
  if (parts.length === 0 || !parts[0]) return { target: null, rest: '' }
  const { all } = await ctx.collectInstances()
  if (all.some((i) => i.name === parts[0])) {
    return { target: parts[0], rest: parts.slice(1).join(' ') }
  }
  return { target: null, rest: args.trim() }
}

// ============================================================================
// 命令表 + 别名
// ============================================================================

interface CommandEntry {
  run: CommandFn
  aliases: string[]
}

const COMMANDS: Record<string, CommandEntry> = {
  instances: { run: cmdInstances, aliases: ['实例列表', '实力列表', '所有实例', '全部实例', '正在运行的实例', '实例', '实例列', '学历列表', '学力列表'] },
  use: { run: cmdUse, aliases: ['切换', '切换到', '切到'] },
  cmd: { run: cmdSendCommand, aliases: ['发送命令', '执行命令'] },
  msg: { run: cmdSendMessage, aliases: ['发送消息', '发消息'] },
}

/** 检查文本是否是命令（斜杠 / 中文别名），返回规范化命令名 */
export function normalizeCommand(text: string): { name: string; rest: string } | null {
  let trimmed = text.trim()
  // 去掉语音转文字前缀
  trimmed = trimmed.replace(/^\[语音转文字\]\s*/, '')
  // 去掉语气词前缀
  trimmed = trimmed.replace(/^(?:嗯|唔|不|啊|哦|好|对)[，,、\s]+/, '')
  trimmed = trimmed.replace(/[。！？!?.,，、；;：:\s]+$/u, '')
  trimmed = trimmed.replace(/^／/, '/')

  let name: string | null = null
  let rest = ''

  if (trimmed.startsWith('/')) {
    const [cmd, ...parts] = trimmed.slice(1).split(/\s+/)
    name = cmd.toLowerCase()
    rest = parts.join(' ')
  } else {
    // 中文别名：完全匹配
    for (const [cmd, entry] of Object.entries(COMMANDS)) {
      if (entry.aliases.includes(trimmed)) {
        name = cmd
        rest = ''
        break
      }
      // 别名前缀匹配：如「切到实例2」「切换到 pigw」
      for (const alias of entry.aliases) {
        if (alias !== '切换' && alias !== '切换到' && alias !== '切到' && trimmed.startsWith(alias)) {
          name = cmd
          rest = trimmed.slice(alias.length).trim()
          break
        }
      }
      if (name) break
    }
  }

  if (!name || !COMMANDS[name]) return null
  return { name, rest }
}

export async function executeCommand(text: string, ctx: CommandCtx): Promise<CommandResult | null> {
  const norm = normalizeCommand(text)
  if (!norm) return null
  const entry = COMMANDS[norm.name]
  if (!entry) return null
  const result = await entry.run(norm.rest, ctx)
  return result
}
