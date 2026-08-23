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
  /** 在目标实例所在机器的 tmux 中启动 pi（新会话 pi-<实例名>） */
  doStartPi: (target: InstanceInfo, cwd?: string) => Promise<string>
  /** 重载所有实例（当前实例本地执行，其他实例发指令），返回汇总 */
  doReloadAll: () => Promise<string>
  /** 写入系统剪贴板（本机 pbcopy/xclip） */
  writeClipboard: (text: string) => Promise<boolean>
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
    const host = inst.host ? `@${inst.host}` : ''
    // cwd 列在行尾（远程实例 cwd 可能为空则不显示）
    const cwd = inst.cwd ? ` ${inst.cwd}` : ''
    return `${inst.name}${host}${mark}${cwd}`
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

export interface StartPiTarget {
  inst: InstanceInfo
  rest: string
}

/**
 * start-pi 参数解析：第一参数匹配实例名 → 指定实例；否则默认当前实例（本机）。
 * 第一参数含 / ~ 或以 . 开头视为目录；否则像实例名但找不到 → 报错（避免打错实例名被静默当目录）。
 */
export function parseStartPiTarget(
  all: InstanceInfo[],
  currentInstanceName: string,
  args: string,
): StartPiTarget | { error: string } {
  const parts = args.trim().split(/\s+/)
  const first = parts[0] ?? ''
  const looksLikeDir = first.includes('/') || first.includes('~') || first.startsWith('.')
  if (first && !looksLikeDir) {
    const hit = all.find((i) => i.name === first)
    if (hit) return { inst: hit, rest: parts.slice(1).join(' ') }
    return { error: `未找到实例 ${first}，先 /instances 查看` }
  }
  const inst = all.find((i) => i.name === currentInstanceName)
  if (!inst) return { error: '未找到当前实例' }
  return { inst, rest: args.trim() }
}

async function cmdStartPi(args: string, ctx: CommandCtx): Promise<CommandResult> {
  const { all } = await ctx.collectInstances()
  const parsed = parseStartPiTarget(all, ctx.currentInstanceName, args)
  if ('error' in parsed) return { reply: parsed.error, consumed: true }
  const cwd = parsed.rest.trim() || undefined
  return { reply: await ctx.doStartPi(parsed.inst, cwd), consumed: true }
}

async function cmdReloadAll(_args: string, ctx: CommandCtx): Promise<CommandResult> {
  return { reply: await ctx.doReloadAll(), consumed: true }
}

async function cmdClipboard(args: string, ctx: CommandCtx): Promise<CommandResult> {
  const content = args.trim()
  if (!content) return { reply: '用法: /clipboard <内容>（复制内容到本机剪贴板）', consumed: true }
  const ok = await ctx.writeClipboard(content)
  return { reply: ok ? `✅ 已复制 ${content.length} 字符到剪贴板` : '❌ 剪贴板写入失败（本机无 pbcopy/xclip）', consumed: true }
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
// 文本清理（语音转文字前缀 / 语气词前缀 / 尾部标点 / 全角斜杠）
// ============================================================================

function cleanCommandText(text: string): string {
  let trimmed = text.trim()
  // 去掉语音转文字前缀
  trimmed = trimmed.replace(/^\[语音转文字\]\s*/, '')
  // 去掉语气词前缀
  trimmed = trimmed.replace(/^(?:嗯|唔|不|啊|哦|好|对)[，,、\s]+/, '')
  trimmed = trimmed.replace(/[。！？!?.,，、；;：:\s]+$/u, '')
  trimmed = trimmed.replace(/^／/, '/')
  return trimmed
}

// ============================================================================
// 宽松 use 匹配：纯数字 / use xxx / 整句实例名 → 切换命令
// 基于实例列表上下文校验（编号在范围内、实例数 >= 2），避免误吞普通对话；
// 命中后由命令系统直接消费，不再作为普通消息投递给 agent。
// ============================================================================

export function tryLooseUse(text: string, all: InstanceInfo[]): { name: 'use'; rest: string } | null {
  const trimmed = cleanCommandText(text)
  if (!trimmed || trimmed.startsWith('/')) return null

  // 1) 纯数字（阿拉伯 / 中文数字）：仅在编号有效且实例数 >= 2 时视为切换
  if (/^\d{1,2}$/.test(trimmed) || parseChineseNumber(trimmed) !== null) {
    const n = toNumber(trimmed)
    if (Number.isFinite(n) && n >= 1 && n <= all.length && all.length >= 2) {
      return { name: 'use', rest: String(n) }
    }
    return null
  }

  // 2) 无斜杠英文形式：use <实例名/编号>
  const useMatch = trimmed.match(/^use\s+(.+)$/i)
  if (useMatch) return { name: 'use', rest: useMatch[1].trim() }

  // 3) 整句恰好等于某实例名（忽略大小写）
  const lower = trimmed.toLowerCase()
  const hit = all.find((i) => i.name.toLowerCase() === lower)
  if (hit) return { name: 'use', rest: hit.name }
  return null
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
  'start-pi': { run: cmdStartPi, aliases: ['start pi', 'start-pi', '启动pi', '启动派', '启动皮', '启动Pi', '启动一个pi', '开pi', '开个pi'] },
  reloadall: { run: cmdReloadAll, aliases: ['重载全部', '全部重载', 'reload all', 'reloadall', '重载所有', '重启全部'] },
  clipboard: { run: cmdClipboard, aliases: ['复制', '拷贝', '复制到剪贴板'] },
}

/** 检查文本是否是命令（斜杠 / 中文别名），返回规范化命令名 */
export function normalizeCommand(text: string): { name: string; rest: string } | null {
  const trimmed = cleanCommandText(text)

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

export interface ExecuteOptions {
  /** 是否启用宽松 use 匹配（纯数字 / use xxx / 整句实例名 → 切换）。
   *  渠道正在等待问卷答案时应置 false：用户发的数字可能真是答案，不能当切换命令消费。 */
  loose?: boolean
}

export async function executeCommand(text: string, ctx: CommandCtx, opts: ExecuteOptions = {}): Promise<CommandResult | null> {
  const norm = normalizeCommand(text)
  if (norm) {
    const entry = COMMANDS[norm.name]
    if (!entry) return null
    return await entry.run(norm.rest, ctx)
  }
  // 宽松 use：说「2」「use home」「home」→ 直接执行切换并消费，不投递给 agent
  if (opts.loose !== false) {
    const { all } = await ctx.collectInstances()
    const loose = tryLooseUse(text, all)
    if (loose) return await COMMANDS.use.run(loose.rest, ctx)
  }
  return null
}
