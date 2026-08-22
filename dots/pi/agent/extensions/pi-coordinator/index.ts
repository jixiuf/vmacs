// ============================================================================
// pi-coordinator — pi session 间协调插件
// 实例注册、接管切换、指令互发（/new /reload 等）、普通消息互发。
// 独立于 pi-wechat-assistant；后者可通过 globalThis.__PI_COORDINATOR__ 复用。
// ============================================================================

import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import * as net from 'node:net'

// Load marker: proves whether this file was actually (re)loaded by pi.
// Written at module evaluation time, before any config or timers.
try {
  fs.appendFileSync('/tmp/pi-coordinator-msg.log', `[${new Date().toISOString()}] EXT LOADED pid=${process.pid}\n`)
} catch {
  // ignore
}
import { Type } from '@sinclair/typebox'
// @ts-ignore — @earendil-works is the current package, but the older package still carries TS declarations used for compatibility here
import type { ExtensionAPI, ExtensionContext, ExtensionCommandContext } from '@mariozechner/pi-coding-agent'
import {
  coordinatorTryLock as coordinatorTryLockLocal,
  coordinatorReleaseLock as coordinatorReleaseLockLocal,
  registerInstance,
  unregisterInstance,
  listInstances,
  listRemoteInstances,
  fetchCoordinatorInstances,
  enqueueRemoteTakeover,
  notifyCoordinator,
  writeRemoteTakeoverRequest,
  startCoordinatorServer,
  pollCoordinator,
  readTakeoverRequest,
  clearTakeoverRequest,
  writeLocalTakeoverRequest,
  getGlobalLockHolder,
  sendCommandToRemote,
  sendMessageToRemote,
  fetchMessages,
  readLocalMessages,
  writeLocalMessages,
  consumeLocalMessages,
  type InstanceInfo,
  type TakeoverRequest,
  type CoordinatorMessage,
} from './src/coordinator.js'

type Ctx = ExtensionContext | ExtensionCommandContext

const COORD_CONFIG_DIR = path.join(os.homedir(), '.pi', 'agent', 'coordinator')
const COORD_CONFIG_FILE = path.join(COORD_CONFIG_DIR, 'config.json')
const WECHAT_CONFIG_FILE = path.join(os.homedir(), '.pi', 'agent', 'wechat-assistant', 'config.json')

interface CoordConfig {
  instanceName?: string
  coordinatorPort?: number
  coordinatorUrl?: string
  remoteInstanceNames?: string[]
  remoteHosts?: Record<string, { target: string; port?: number }>
}

function readConfigFile(file: string): Partial<CoordConfig> | null {
  try {
    return JSON.parse(fs.readFileSync(file, 'utf8')) as Partial<CoordConfig>
  } catch {
    return null
  }
}

/** 读取协调配置：优先自己的 config，缺失字段回退 wechat-assistant 配置（通道互通） */
function loadCoordConfig(): CoordConfig {
  const own = readConfigFile(COORD_CONFIG_FILE) ?? {}
  const wechat = readConfigFile(WECHAT_CONFIG_FILE) ?? {}
  const cfg: CoordConfig = {
    instanceName: own.instanceName ?? wechat.instanceName,
    coordinatorPort: own.coordinatorPort ?? wechat.coordinatorPort,
    coordinatorUrl: own.coordinatorUrl ?? wechat.coordinatorUrl,
    remoteInstanceNames: own.remoteInstanceNames ?? wechat.remoteInstanceNames,
    remoteHosts: own.remoteHosts ?? wechat.remoteHosts,
  }
  // 配置文件缺失/损坏时自动重建，避免配置真空（实例名丢失导致消息拉取失效）
  if (!readConfigFile(COORD_CONFIG_FILE)) {
    try {
      fs.mkdirSync(COORD_CONFIG_DIR, { recursive: true })
      fs.writeFileSync(COORD_CONFIG_FILE, JSON.stringify(cfg, null, 2))
    } catch {
      // ignore
    }
  }
  return cfg
}

// --- 中文数字 ---

function parseChineseNumber(s: string): number | null {
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

function toNumber(s: string): number {
  return parseChineseNumber(s) ?? parseInt(s, 10)
}

function isPortInUse(port: number): Promise<boolean> {
  return new Promise((resolve) => {
    const sock = net.connect(port, '127.0.0.1')
    sock.once('connect', () => { sock.destroy(); resolve(true) })
    sock.once('error', () => resolve(false))
  })
}

// ============================================================================

export default function coordinatorExtension(pi: ExtensionAPI) {
  let currentInstanceName = ''
  let coordinatorServer: ReturnType<typeof startCoordinatorServer> | null = null
  let latestCtx: Ctx | null = null
  // config 在模块加载时立即读取：reload 后 session_start 不触发时也能拿到 coordinatorUrl
  let config: CoordConfig = loadCoordConfig()

  const watcherKey = '__PI_COORDINATOR_WATCHER__'
  const g = globalThis as Record<string, unknown>

  // --- 轮询定时器：模块加载即启动（不依赖 session_start，reload 后强制重建） ---
  // 此前定时器仅在 session_start 里启动；reload 时若该事件未触发或旧 watcher 残留，
  // 轮询会永久停止（消息/接管请求不再被消费）。这里每次加载都强制重建，保证可用。
  const oldWatcher = g[watcherKey] as ReturnType<typeof setInterval> | undefined
  if (oldWatcher) clearInterval(oldWatcher)
  const oldAuto = g[`${watcherKey}_AUTO_TAKEOVER`] as ReturnType<typeof setInterval> | undefined
  if (oldAuto) clearInterval(oldAuto)
  g[watcherKey] = setInterval(() => {
    void pollIncoming().catch(() => {})
    void pollMessages().catch(() => {})
  }, 2000)
  g[`${watcherKey}_AUTO_TAKEOVER`] = setInterval(() => {
    void autoTakeoverIfIdle().catch(() => {})
    void ensureCoordinatorIfNeeded().catch(() => {})
  }, 5000)
  try {
    fs.appendFileSync('/tmp/pi-coordinator-msg.log', `[${new Date().toISOString()}] WATCHER STARTED pid=${process.pid}\n`)
  } catch {
    // ignore
  }

  // --- 协调中心故障转移：本实例配置了 coordinatorPort 但被占用降级时，检测到协调中心退出则提升接管 ---

  async function ensureCoordinatorIfNeeded(): Promise<void> {
    try {
      if (!config.coordinatorPort || coordinatorServer) return
      const inUse = await isPortInUse(config.coordinatorPort)
      if (inUse) return
      // 协调中心已退出：本实例提升为协调中心
      coordinatorServer = startCoordinatorServer(
        config.coordinatorPort,
        { name: currentInstanceName, pid: process.pid, cwd: process.cwd(), host: os.hostname() },
        config.remoteInstanceNames ?? [],
      )
      log(`协调中心故障转移：本实例接管端口 ${config.coordinatorPort}`)
      config = { ...config, coordinatorUrl: undefined }
    } catch {
      // ignore
    }
  }

  // --- 自动接管检测：协调锁无人持有（持有者已退出/超时）时，本实例自动接管微信 ---

  let unreachableCount = 0
  async function autoTakeoverIfIdle(): Promise<void> {
    try {
      const baseUrl = config.coordinatorUrl ?? (config.coordinatorPort ? `http://127.0.0.1:${config.coordinatorPort}` : null)
      if (!baseUrl) return
      const res = await fetch(`${baseUrl}/lock`)
      if (!res.ok) {
        unreachableCount = 0
        return
      }
      const d = (await res.json()) as { holder?: { name?: string } | null }
      if (!d.holder || !d.holder.name) {
        unreachableCount = 0
        log(`协调锁空闲（持有者已退出），自动接管微信`)
        writeLocalTakeoverRequest({
          targetName: currentInstanceName,
          targetPid: 0,
          fromName: currentInstanceName,
          capability: 'wechat',
          timestamp: Date.now(),
        })
      } else {
        unreachableCount = 0
      }
    } catch {
      // 协调中心不可达：连续 3 次（约 15s）仍不可达 → 本地降级接管微信（服务不中断）
      unreachableCount++
      if (unreachableCount >= 3) {
        log(`协调中心持续不可达，降级接管微信`)
        writeLocalTakeoverRequest({
          targetName: currentInstanceName,
          targetPid: 0,
          fromName: currentInstanceName,
          capability: 'wechat',
          timestamp: Date.now(),
        })
      }
    }
  }

  // --- 定时器：拉取接管请求 / 命令 / 消息 ---

  async function pollIncoming(): Promise<void> {
    try {
      // 本地接管请求：command 由本扩展处理；wechat/通用接管留给消费方（wechat tryTakeover）读取，
      // 避免与 wechat 抢读导致请求被丢弃
      const req = readTakeoverRequest()
      if (req && req.capability === 'command') {
        clearTakeoverRequest()
        await handleTakeoverRequest(req)
      }
      // 协调中心拉取（局域网机器）：拉到的请求写入本地 takeover.json 供消费方（如 wechat tryTakeover）使用
      if (config.coordinatorUrl) {
        const remoteReq = await pollCoordinator(config.coordinatorUrl, currentInstanceName, process.pid)
        if (remoteReq && (remoteReq.targetPid === 0 || remoteReq.targetPid === process.pid)) {
          writeLocalTakeoverRequest(remoteReq)
          await handleTakeoverRequest(remoteReq)
        }
      }
    } catch {
      // ignore
    }
  }

  async function handleTakeoverRequest(req: TakeoverRequest): Promise<void> {
    if (req.capability === 'command') {
      const command = (req.payload as { command?: string } | undefined)?.command
      if (command) {
        log(`收到来自 ${req.fromName} 的指令: ${command}`)
        await pi.sendUserMessage(command, {
          deliverAs: 'followUp',
          expandPromptTemplates: true,
        } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
      }
      return
    }
    // 通用接管：通知订阅者（globalThis 桥）
    log(`收到接管请求: ${req.targetName} (capability=${req.capability ?? 'default'})`)
    const bridge = g.__PI_COORDINATOR__ as { onTakeover?: (req: TakeoverRequest) => void } | undefined
    bridge?.onTakeover?.(req)
  }

  // --- 定时器：拉取消息（局域网机器） ---

  async function pollMessages(): Promise<void> {
    try {
      let messages: CoordinatorMessage[] = []
      if (config.coordinatorUrl) {
        // 局域网机器：从协调中心拉取消息（服务器端已删，不会重复）
        messages = await fetchMessages(config.coordinatorUrl, currentInstanceName)
      } else if (config.coordinatorPort) {
        // 服务器模式：消费本地消息文件中发给本实例的消息（读即删，不重复）
        messages = consumeLocalMessages(currentInstanceName)
      } else {
        return
      }
      for (const m of messages) {
        log(`收到来自 ${m.from} 的消息: ${m.text.slice(0, 50)}`)
        try {
          fs.appendFileSync('/tmp/pi-coordinator-msg.log', `[${new Date().toISOString()}] RECV from=${m.from} text=${m.text.slice(0, 80)} id=${m.id}\n`)
        } catch {
          // ignore
        }
        const bridge = g.__PI_COORDINATOR__ as { onMessage?: (m: CoordinatorMessage) => void } | undefined
        try {
          // 统一交给桥订阅者（pi-wechat-assistant 的 onMessage）投递给 agent，避免重复投递
          bridge?.onMessage?.(m)
        } catch {
          // ignore
        }
      }
    } catch {
      // ignore
    }
  }

  // 默认静默，避免 console.log 污染 TUI；调试时设 PI_COORDINATOR_DEBUG=1 输出
  const isDebug = !!process.env.PI_COORDINATOR_DEBUG
  function log(msg: string): void {
    if (isDebug) console.log(`[pi-coordinator] ${msg}`)
  }

  function ok(text: string) {
    return { content: [{ type: 'text' as const, text }], details: {} }
  }

  function fail(text: string) {
    return { content: [{ type: 'text' as const, text: `❌ ${text}` }], details: {} }
  }

  // --- 工具：列出实例 ---

  pi.registerTool({
    name: 'list_instances',
    label: 'List Instances',
    description: '列出所有 pi 实例（本机 + 远程），标注当前实例。供实例间协调、切换前查看。',
    promptSnippet: '列出所有 pi 实例',
    promptGuidelines: ['用户询问实例状态、切换前先调用本工具。编号可用于 switch_instance 的 target。'],
    parameters: Type.Object({}),
    async execute() {
      try {
        const remoteCfg = config.remoteHosts ?? {}
        const local = listInstances()
        const all: InstanceInfo[] = [...local]
        if (config.coordinatorPort) {
          for (const nm of config.remoteInstanceNames ?? []) {
            if (!all.some((i) => i.name === nm)) all.push({ name: nm, pid: 0, cwd: '', sessionId: '', lastSeen: 0 })
          }
        }
        if (config.coordinatorUrl) {
          const ci = await fetchCoordinatorInstances(config.coordinatorUrl)
          if (ci) {
            for (const li of ci.local) {
              if (!all.some((i) => i.name === li.name)) all.push({ name: li.name, pid: li.pid, cwd: li.cwd, sessionId: '', lastSeen: 0 })
            }
          }
        }
        try {
          const sshRemote = await listRemoteInstances(remoteCfg)
          for (const r of sshRemote) {
            if (!all.some((i) => i.name === r.name)) all.push(r)
          }
        } catch {
          // ignore
        }
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

  // --- 工具：切换接管 ---

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
        const name = String(params.target).trim()
        const capability = params.capability ?? 'wechat'
        const local = listInstances()
        const all: InstanceInfo[] = [...local]
        if (config.coordinatorPort) {
          for (const nm of config.remoteInstanceNames ?? []) {
            if (!all.some((i) => i.name === nm)) all.push({ name: nm, pid: 0, cwd: '', sessionId: '', lastSeen: 0 })
          }
        }
        if (config.coordinatorUrl) {
          const ci = await fetchCoordinatorInstances(config.coordinatorUrl)
          if (ci) {
            for (const li of ci.local) {
              if (!all.some((i) => i.name === li.name)) all.push({ name: li.name, pid: li.pid, cwd: li.cwd, sessionId: '', lastSeen: 0 })
            }
          }
        }
        let target: InstanceInfo | undefined
        const num = toNumber(name)
        if (Number.isFinite(num) && num >= 1) target = all[num - 1]
        else target = all.find((i) => i.name === name)
        if (!target) return fail(`未找到实例 ${name}，请先调用 list_instances 查看`)
        if (target.pid === process.pid) return ok(`已经在当前实例（${target.name}）`)

        const req: TakeoverRequest = {
          targetName: target.name,
          targetPid: target.pid,
          fromName: currentInstanceName || 'local',
          capability,
          timestamp: Date.now(),
        }
        // 协调中心：服务器 → 局域网
        if (config.coordinatorPort && (config.remoteInstanceNames ?? []).includes(target.name)) {
          enqueueRemoteTakeover(req)
          return ok(`已向实例 ${target.name} 发送接管请求`)
        }
        // 协调中心：局域网 → 服务器
        if (config.coordinatorUrl && !local.some((i) => i.name === target.name)) {
          await notifyCoordinator(config.coordinatorUrl, req)
          return ok(`已向服务器实例 ${target.name} 发送接管请求`)
        }
        // SSH
        const remoteTarget = config.remoteHosts?.[target.name]
        if (remoteTarget) {
          await writeRemoteTakeoverRequest(remoteTarget, req)
          return ok(`已向远程实例 ${target.name} 发送接管请求`)
        }
        return fail(`实例 ${target.name} 没有可用的通道`)
      } catch (err) {
        return fail(`切换失败: ${(err as Error).message}`)
      }
    },
  })

  // --- 工具：发送指令（/new /reload /compact 等） ---

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
        const target = String(params.target).trim()
        const command = String(params.command).trim()
        const req: TakeoverRequest = {
          targetName: target,
          targetPid: 0,
          fromName: currentInstanceName || 'local',
          capability: 'command',
          payload: { command },
          timestamp: Date.now(),
        }
        if (config.coordinatorPort && (config.remoteInstanceNames ?? []).includes(target)) {
          enqueueRemoteTakeover(req)
        } else if (config.coordinatorUrl) {
          await sendCommandToRemote(config.coordinatorUrl, target, command, currentInstanceName)
        } else if (config.remoteHosts?.[target]) {
          await writeRemoteTakeoverRequest(config.remoteHosts[target], req)
        } else {
          // 本机实例
          writeLocalTakeoverRequest(req)
        }
        return ok(`已向实例 ${target} 发送指令: ${command}`)
      } catch (err) {
        return fail(`发送指令失败: ${(err as Error).message}`)
      }
    },
  })

  // --- 工具：发送普通消息 ---

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
        const target = String(params.target).trim()
        const text = String(params.text).trim()
        const from = currentInstanceName || 'local'
        if (config.coordinatorPort) {
          const messages = readLocalMessages()
          messages.push({ id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`, from, to: target, text, timestamp: Date.now() })
          writeLocalMessages(messages)
        } else if (config.coordinatorUrl) {
          await sendMessageToRemote(config.coordinatorUrl, target, text, from)
        }
        return ok(`已向实例 ${target} 发送消息`)
      } catch (err) {
        return fail(`发送消息失败: ${(err as Error).message}`)
      }
    },
  })

  // --- 公共逻辑（工具与 / 命令共用） ---

  async function collectInstances(): Promise<{ local: InstanceInfo[]; all: InstanceInfo[] }> {
    const remoteCfg = config.remoteHosts ?? {}
    const local = listInstances()
    const all: InstanceInfo[] = [...local]
    if (config.coordinatorPort) {
      for (const nm of config.remoteInstanceNames ?? []) {
        if (!all.some((i) => i.name === nm)) all.push({ name: nm, pid: 0, cwd: '', sessionId: '', lastSeen: 0 })
      }
    }
    if (config.coordinatorUrl) {
      const ci = await fetchCoordinatorInstances(config.coordinatorUrl)
      if (ci) {
        for (const li of ci.local) {
          if (!all.some((i) => i.name === li.name)) all.push({ name: li.name, pid: li.pid, cwd: li.cwd, sessionId: '', lastSeen: 0, host: li.host })
        }
        // 其他活跃客户端（同机/远端通过协调中心连接的实例）
        for (const c of ci.clients ?? []) {
          if (!all.some((i) => i.name === c.name)) all.push({ name: c.name, pid: 0, cwd: c.cwd ?? '', sessionId: '', lastSeen: 0, host: c.host })
        }
      }
    }
    try {
      const sshRemote = await listRemoteInstances(remoteCfg)
      for (const r of sshRemote) {
        if (!all.some((i) => i.name === r.name)) all.push(r)
      }
    } catch {
      // ignore
    }
    return { local, all }
  }

  function resolveTarget(all: InstanceInfo[], name: string): InstanceInfo | undefined {
    const num = toNumber(name)
    if (Number.isFinite(num) && num >= 1) return all[num - 1]
    return all.find((i) => i.name === name)
  }

  /** 命令参数补全：返回实例名候选（远程实例 label 带 hostname） */
  const instanceCompletions = async (argumentPrefix: string) => {
    const { local, all } = await collectInstances()
    return all
      .filter((i) => i.name.startsWith(argumentPrefix))
      .map((i) => {
        const isRemote = !local.some((l) => l.name === i.name)
        const label = isRemote ? (i.host ? `${i.name}@${i.host}` : `${i.name}（远程）`) : i.name
        const description = i.host ? `@${i.host} · ${i.cwd || '(远程)'}` : i.cwd
        return { value: i.name, label, description }
      })
  }

  // 记住上次指定的实例名（/use /cmd /msg 不指定实例时复用）
  let lastTargetName: string | null = null

  function rememberTarget(name: string): void {
    lastTargetName = name
  }

  async function isKnownInstance(name: string): Promise<boolean> {
    const { all } = await collectInstances()
    return all.some((i) => i.name === name)
  }

  /** /cmd /msg 解析：参数 1 若匹配已知实例名则视为实例，否则复用上次实例并把全部参数当内容 */
  async function parseTargetArgs(args: string): Promise<{ target: string | null; rest: string }> {
    const parts = args.trim().split(/\s+/)
    if (parts.length === 0) return { target: null, rest: '' }
    if (await isKnownInstance(parts[0])) {
      return { target: parts[0], rest: parts.slice(1).join(' ') }
    }
    return { target: null, rest: args.trim() }
  }

  async function doSwitch(name: string, capability?: string): Promise<string> {
    const { local, all } = await collectInstances()
    const target = resolveTarget(all, name)
    if (!target) return `未找到实例 ${name}，先 /instances 查看`
    const cap = capability ?? 'wechat'
    // 目标是自己：若当前本机已在接管则无需操作；否则发起接管请求（让当前持有者让位）
    if (target.pid === process.pid) {
      const holder = getGlobalLockHolder()
      if (holder?.name === currentInstanceName) return `微信已由 ${target.name} 接管`
      if (config.coordinatorUrl) {
        await notifyCoordinator(config.coordinatorUrl, {
          targetName: target.name,
          targetPid: 0,
          fromName: currentInstanceName || 'local',
          capability: cap,
          timestamp: Date.now(),
        })
        rememberTarget(target.name)
        return `已请求 ${target.name} 接管微信`
      }
      return `已经在当前实例（${target.name}），但无协调通道可发起接管`
    }
    rememberTarget(target.name)
    const req: TakeoverRequest = {
      targetName: target.name,
      targetPid: target.pid,
      fromName: currentInstanceName || 'local',
      capability: cap,
      timestamp: Date.now(),
    }
    if (config.coordinatorPort && (config.remoteInstanceNames ?? []).includes(target.name)) {
      enqueueRemoteTakeover(req)
      return `已向实例 ${target.name} 发送接管请求`
    }
    if (config.coordinatorUrl && !local.some((i) => i.name === target.name)) {
      await notifyCoordinator(config.coordinatorUrl, req)
      return `已向服务器实例 ${target.name} 发送接管请求`
    }
    const remoteTarget = config.remoteHosts?.[target.name]
    if (remoteTarget) {
      await writeRemoteTakeoverRequest(remoteTarget, req)
      return `已向远程实例 ${target.name} 发送接管请求`
    }
    return `实例 ${target.name} 没有可用的通道`
  }

  async function doSendCommand(target: string, command: string): Promise<string> {
    rememberTarget(target)
    const req: TakeoverRequest = {
      targetName: target,
      targetPid: 0,
      fromName: currentInstanceName || 'local',
      capability: 'command',
      payload: { command },
      timestamp: Date.now(),
    }
    if (config.coordinatorPort && (config.remoteInstanceNames ?? []).includes(target)) {
      enqueueRemoteTakeover(req)
    } else if (config.coordinatorUrl) {
      await sendCommandToRemote(config.coordinatorUrl, target, command, currentInstanceName)
    } else if (config.remoteHosts?.[target]) {
      await writeRemoteTakeoverRequest(config.remoteHosts[target], req)
    } else {
      writeLocalTakeoverRequest(req)
    }
    return `已向实例 ${target} 发送指令: ${command}`
  }

  async function doSendMessage(target: string, text: string): Promise<string> {
    rememberTarget(target)
    const from = currentInstanceName || 'local'
    if (config.coordinatorPort) {
      const messages = readLocalMessages()
      messages.push({ id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`, from, to: target, text, timestamp: Date.now() })
      writeLocalMessages(messages)
    } else if (config.coordinatorUrl) {
      await sendMessageToRemote(config.coordinatorUrl, target, text, from)
    }
    return `已向实例 ${target} 发送消息`
  }

  // --- TUI 斜杠命令 ---

  pi.registerCommand('instances', {
    description: '列出所有 pi 实例（本机 + 远程）',
    handler: async (_args, ctx) => {
      const { local, all } = await collectInstances()
      if (all.length === 0) {
        ctx.ui.notify('没有登记的实例', 'warning')
        return
      }
      const lines = all.map((inst) => {
        const marks: string[] = []
        if (inst.pid === process.pid) marks.push('当前')
        if (!local.some((l) => l.name === inst.name)) marks.push('远程')
        const mark = marks.length > 0 ? `（${marks.join('，')}）` : ''
        return `${inst.name}${inst.host ? '@' + inst.host : ''}${mark}`
      })
      ctx.ui.notify(`实例列表：\n${lines.map((l, i) => `${i + 1}. ${l}`).join('\n')}`, 'info')
    },
  })

  pi.registerCommand('use', {
    getArgumentCompletions: instanceCompletions,
    description: '把接管权切换到指定实例（实例名/编号/默认复用上次）',
    handler: async (args, ctx) => {
      const name = args.trim()
      if (!name) {
        if (lastTargetName) {
          ctx.ui.notify(await doSwitch(lastTargetName, 'wechat'), 'info')
          return
        }
        ctx.ui.notify('用法: /use <实例名或编号>，如 /use pigw 或 /use 2', 'warning')
        return
      }
      ctx.ui.notify(await doSwitch(name, 'wechat'), 'info')
    },
  })

  const notifyCmdResult = (ctx: { ui: { notify: (m: string, t?: 'info' | 'warning' | 'error') => void } }, result: string) => {
    ctx.ui.notify(result, 'info')
  }


  pi.registerCommand('send-command', {
    getArgumentCompletions: instanceCompletions,
    description: '向实例发送指令：/send-command [实例名] <指令>（不写实例名复用上次）',
    handler: async (args, ctx) => {
      const { target, rest } = await parseTargetArgs(args)
      if (!rest) {
        ctx.ui.notify('用法: /send-command [实例名] <指令>，如 /send-command /reload 或 /send-command agent /reload', 'warning')
        return
      }
      if (target) {
        notifyCmdResult(ctx, await doSendCommand(target, rest))
      } else if (lastTargetName) {
        notifyCmdResult(ctx, await doSendCommand(lastTargetName, rest))
      } else {
        ctx.ui.notify('未指定实例且没有上次实例，先 /use <实例> 或 /send-command <实例> <指令>', 'warning')
      }
    },
  })

  pi.registerCommand('send-message', {
    getArgumentCompletions: instanceCompletions,
    description: '向实例发送消息：/send-message [实例名] <内容>（不写实例名复用上次）',
    handler: async (args, ctx) => {
      const { target, rest } = await parseTargetArgs(args)
      if (!rest) {
        ctx.ui.notify('用法: /send-message [实例名] <内容>，如 /send-message hello 或 /send-message agent hello', 'warning')
        return
      }
      if (target) {
        notifyCmdResult(ctx, await doSendMessage(target, rest))
      } else if (lastTargetName) {
        notifyCmdResult(ctx, await doSendMessage(lastTargetName, rest))
      } else {
        ctx.ui.notify('未指定实例且没有上次实例，先 /use <实例> 或 /send-message <实例> <内容>', 'warning')
      }
    },
  })

  // 别名：/cmd = /send-command，/msg = /send-message
  pi.registerCommand('cmd', {
    getArgumentCompletions: instanceCompletions,
    description: '别名：/send-command',
    handler: async (args, ctx) => {
      const { target, rest } = await parseTargetArgs(args)
      if (!rest) {
        ctx.ui.notify('用法: /cmd [实例名] <指令>', 'warning')
        return
      }
      if (target) notifyCmdResult(ctx, await doSendCommand(target, rest))
      else if (lastTargetName) notifyCmdResult(ctx, await doSendCommand(lastTargetName, rest))
      else ctx.ui.notify('未指定实例且无上次实例', 'warning')
    },
  })

  pi.registerCommand('msg', {
    getArgumentCompletions: instanceCompletions,
    description: '别名：/send-message',
    handler: async (args, ctx) => {
      const { target, rest } = await parseTargetArgs(args)
      if (!rest) {
        ctx.ui.notify('用法: /msg [实例名] <内容>', 'warning')
        return
      }
      if (target) notifyCmdResult(ctx, await doSendMessage(target, rest))
      else if (lastTargetName) notifyCmdResult(ctx, await doSendMessage(lastTargetName, rest))
      else ctx.ui.notify('未指定实例且无上次实例', 'warning')
    },
  })

  // --- 会话生命周期 ---

  pi.on('session_start', async (_event, ctx) => {
    latestCtx = ctx
    config = loadCoordConfig()
    try {
      fs.appendFileSync('/tmp/pi-coordinator-msg.log', `[${new Date().toISOString()}] SESSION_START reason=${(_event as { reason?: string } | undefined)?.reason ?? '?'} cwd=${ctx.cwd} url=${config.coordinatorUrl ?? 'none'}\n`)
    } catch {
      // ignore
    }
    // 实例名用工作目录名（同机多实例天然唯一）；config.instanceName 优先覆盖（如跨机固定名称）
    currentInstanceName = config.instanceName || path.basename(ctx.cwd) || 'pi'

    registerInstance({
      name: currentInstanceName,
      pid: process.pid,
      cwd: ctx.cwd,
      sessionId: ctx.sessionManager.getSessionId(),
      host: os.hostname(),
    })

    // 协调中心（服务器侧）：端口被占（同机已有协调中心）→ 降级为客户端接入，不崩溃
    if (config.coordinatorPort && !g[watcherKey]) {
      const inUse = await isPortInUse(config.coordinatorPort)
      if (!inUse) {
        coordinatorServer = startCoordinatorServer(
          config.coordinatorPort,
          { name: currentInstanceName, pid: process.pid, cwd: ctx.cwd, host: os.hostname() },
          config.remoteInstanceNames ?? [],
        )
        log(`协调中心已启动: 端口 ${config.coordinatorPort}`)
      } else {
        log(`协调端口 ${config.coordinatorPort} 已被占用，本实例作为客户端接入 (127.0.0.1:${config.coordinatorPort})`)
        config = { ...config, coordinatorUrl: `http://127.0.0.1:${config.coordinatorPort}` }
      }
    }

    // 兜底：若定时器因 reload 顺序异常被 session_shutdown 误清，此处补启动
    if (!g[watcherKey]) {
      g[watcherKey] = setInterval(() => {
        void pollIncoming().catch(() => {})
        void pollMessages().catch(() => {})
      }, 2000)
      g[`${watcherKey}_AUTO_TAKEOVER`] = setInterval(() => {
        void autoTakeoverIfIdle().catch(() => {})
        void ensureCoordinatorIfNeeded().catch(() => {})
      }, 5000)
    }
  })


  pi.on('session_shutdown', async () => {
    const watcher = g[watcherKey] as ReturnType<typeof setInterval> | undefined
    if (watcher) {
      clearInterval(watcher)
      delete g[watcherKey]
    }
    const autoTakeover = g[`${watcherKey}_AUTO_TAKEOVER`] as ReturnType<typeof setInterval> | undefined
    if (autoTakeover) {
      clearInterval(autoTakeover)
      delete g[`${watcherKey}_AUTO_TAKEOVER`]
    }
    if (coordinatorServer) {
      try {
        coordinatorServer.close()
      } catch {
        // ignore
      }
      coordinatorServer = null
    }
    if (currentInstanceName) unregisterInstance(currentInstanceName, process.pid)
  })

  // --- globalThis 桥：供 pi-wechat-assistant 等扩展复用 ---
  // 保留已注册的 onTakeover/onMessage（扩展加载顺序不定，避免覆盖对方注册的回调）

  const existingBridge = g.__PI_COORDINATOR__ as { onTakeover?: unknown; onMessage?: unknown } | undefined
  g.__PI_COORDINATOR__ = {
    version: '0.1.0',
    onTakeover: existingBridge?.onTakeover,
    onMessage: existingBridge?.onMessage,
    coordinatorTryLock: (name: string, pid: number, capability?: string, force = false) =>
      coordinatorTryLockLocal(name, pid, capability, force),
    coordinatorReleaseLock: (name: string, capability?: string) =>
      coordinatorReleaseLockLocal(name, capability),
    registerInstance,
    unregisterInstance,
    listInstances,
    listRemoteInstances,
    fetchCoordinatorInstances,
    enqueueRemoteTakeover,
    notifyCoordinator,
    writeRemoteTakeoverRequest,
    pollCoordinator,
    readTakeoverRequest,
    clearTakeoverRequest,
    writeLocalTakeoverRequest,
    getGlobalLockHolder,
    sendCommandToRemote,
    sendMessageToRemote,
    getConfig: () => loadCoordConfig(),
  }
}
