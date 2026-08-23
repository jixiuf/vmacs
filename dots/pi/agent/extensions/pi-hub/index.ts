// ============================================================================
// pi-hub — pi session 间协调核心
// 实例注册、接管切换、指令/消息互发、广播；IM 渠道通过 IGateway 接入。
// 由 pi-coordinator 演化：协调逻辑收敛于此，渠道退化为纯协议网关。
// ============================================================================

import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import * as net from 'node:net'
import { execFile } from 'node:child_process'

try {
  fs.appendFileSync('/tmp/pi-coordinator-msg.log', `[${new Date().toISOString()}] EXT LOADED pid=${process.pid}\n`)
} catch {
  // ignore
}
import { Type } from '@sinclair/typebox'
// @ts-ignore — @earendil-works is the current package, but the older package still carries TS declarations used for compatibility here
import type { ExtensionAPI, ExtensionContext, ExtensionCommandContext } from '@mariozechner/pi-coding-agent'
import { loadHubConfig, isChannelEnabled, type HubConfig } from './src/config.js'
import { registerInstance, unregisterInstance, listInstances } from './src/registry.js'
import {
  coordinatorTryLock,
  coordinatorReleaseLock,
  getGlobalLockHolder,
  preassignLock,
} from './src/lock.js'
import { EnvelopeQueue } from './src/queue.js'
import {
  startCoordinatorServer,
  fetchInbox,
  postEnvelope,
  fetchCoordinatorInstances,
  listRemoteInstances,
  sshExec,
  writeRemoteTakeoverRequest,
  enqueueRemoteTakeover,
  requestRemoteLock,
  releaseRemoteLock,
  listActiveClients,
  clientHostName,
  readLastMsgLocal,
  writeLastMsgLocal,
  fetchLastMsgRemote,
  pushLastMsgRemote,
  type LastWechatMsg,
  type RemoteHostConfig,
} from './src/transport.js'
import { SessionBridge } from './src/bridge.js'
import { Router } from './src/router.js'
import { executeCommand, toNumber, type CommandCtx } from './src/commands.js'
import type {
  IGateway,
  InboundMessage,
  InstanceInfo,
  TakeoverRequest,
  Envelope,
} from './src/types.js'

type Ctx = ExtensionContext | ExtensionCommandContext

const STATE_DIR = path.join(os.homedir(), '.pi', 'agent', 'wechat-assistant')
const TAKEOVER_FILE = path.join(STATE_DIR, 'takeover.json')
const BROADCAST_FILE = path.join(STATE_DIR, 'broadcast.json')

const TAKEOVER_TTL_MS = 60_000

// ============================================================================

export default function hubExtension(pi: ExtensionAPI) {
  let currentInstanceName = ''
  let coordinatorServer: ReturnType<typeof startCoordinatorServer> | null = null
  let latestCtx: Ctx | null = null
  let config: HubConfig = loadHubConfig()

  const queue = new EnvelopeQueue()
  const bridge = new SessionBridge({ pi, deliverToAgent: deliverToAgent })
  const router = new Router({
    handleCommand: handleCommand,
    handleMessage: handleMessage,
    handleTakeover: handleTakeoverEnvelope,
    getGateway: (channel) => gateways.get(channel),
  })
  bridge.setInboundHandler((m) => router.routeInbound(m))

  const watcherKey = '__PI_COORDINATOR_WATCHER__'
  const g = globalThis as Record<string, unknown>

  // --- 渠道注册表（IGateway） ---
  const gateways = new Map<string, IGateway>()
  /** 接管让位回调：capability 被请求接管时通知渠道 */
  const takeoverCallbacks = new Map<string, (req: TakeoverRequest) => void>()

  function registerGateway(gw: IGateway): void {
    gateways.set(gw.kind, gw)
    gw.onInbound((m) => bridge.handleInbound(m))
  }

  function onTakeoverRequest(cb: (req: TakeoverRequest) => void): void {
    const key = `cb-${takeoverCallbacks.size}`
    takeoverCallbacks.set(key, cb)
  }

  // --- 轮询定时器：模块加载即启动（不依赖 session_start，reload 后强制重建） ---
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

  // --- 协调中心故障转移 ---

  async function ensureCoordinatorIfNeeded(): Promise<void> {
    try {
      if (!config.coordinatorPort || coordinatorServer) return
      const inUse = await isPortInUse(config.coordinatorPort)
      if (inUse) return
      coordinatorServer = startCoordinatorServer(
        config.coordinatorPort,
        { name: currentInstanceName, pid: process.pid, cwd: process.cwd(), host: os.hostname() },
        config.remoteInstanceNames ?? [],
        queue,
      )
      log(`协调中心故障转移：本实例接管端口 ${config.coordinatorPort}`)
      config = { ...config, coordinatorUrl: undefined }
    } catch {
      // ignore
    }
  }

  // --- 自动接管检测 ---

  let unreachableCount = 0
  async function autoTakeoverIfIdle(): Promise<void> {
    try {
      // 仅协调中心模式（端口在本地）负责"锁空闲自动接管"；
      // 客户端模式由协调中心统一仲裁，不自发抢锁（避免多客户端轮番争抢）
      if (config.coordinatorUrl) return
      const baseUrl = config.coordinatorPort ? `http://127.0.0.1:${config.coordinatorPort}` : null
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
        // 通知渠道自动接管（capability=wechat，target 为空表示本机接管）
        notifyAutoTakeover('wechat')
      } else {
        unreachableCount = 0
      }
    } catch {
      // 协调中心不可达：仅协调中心模式自身不适用；客户端降级由 wechat lock() 处理
      unreachableCount++
      if (unreachableCount >= 3 && !config.coordinatorUrl) {
        log(`协调中心持续不可达，降级接管微信`)
        notifyAutoTakeover('wechat')
      }
    }
  }

  /** 自动接管：通知已注册的渠道（如 wechat）启动轮询 */
  function notifyAutoTakeover(capability: string): void {
    for (const cb of takeoverCallbacks.values()) {
      try {
        cb({ targetName: currentInstanceName, targetPid: 0, fromName: currentInstanceName, capability, timestamp: Date.now() })
      } catch {
        // ignore
      }
    }
  }

  // --- 轮询：接管请求 / 指令 / 消息（本地 + 协调中心） ---

  async function pollIncoming(): Promise<void> {
    try {
      const req = readTakeoverRequest()
      if (req && req.capability === 'command') {
        clearTakeoverRequest()
        await handleTakeover(req)
      }
      // 本机广播文件：任意实例写入，本机其他实例各自 ack 后 reload
      consumeLocalBroadcast()
      if (config.coordinatorUrl) {
        const envelopes = await fetchInbox(config.coordinatorUrl, currentInstanceName, os.hostname())
        for (const env of envelopes) {
          routeEnvelope(env)
        }
      }
    } catch {
      // ignore
    }
  }

  /** 消费本机广播文件（broadcast.json）：本实例 ack 后触发 reload */
  function consumeLocalBroadcast(): void {
    try {
      const req = readBroadcastRequest()
      if (!req) return
      if (req.command !== 'reload') return
      if (!ackBroadcastRequest()) return
      log(`收到本机广播命令: ${req.command}，重载扩展`)
      void pi.sendUserMessage('/__hub_reload', {
        deliverAs: 'steer',
        expandPromptTemplates: true,
      } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean }).catch(() => {})
    } catch {
      // ignore
    }
  }

  function routeEnvelope(env: Envelope): void {
    switch (env.type) {
      case 'message': {
        log(`收到来自 ${env.from} 的消息: ${env.text.slice(0, 50)}`)
        try {
          fs.appendFileSync('/tmp/pi-coordinator-msg.log', `[${new Date().toISOString()}] RECV from=${env.from} text=${env.text.slice(0, 80)} id=${env.id}\n`)
        } catch {
          // ignore
        }
        bridge.handleInbound({
          id: env.id,
          channel: 'coord',
          userId: env.from,
          text: env.text,
          ts: env.ts,
        })
        break
      }
      case 'command': {
        log(`收到来自 ${env.from} 的指令: ${env.command}`)
        void pi.sendUserMessage(env.command, {
          deliverAs: 'steer',
          expandPromptTemplates: true,
        } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean }).catch(() => {})
        break
      }
      case 'takeover': {
        handleTakeover({
          targetName: env.to,
          targetPid: 0,
          fromName: env.from,
          capability: env.capability,
          timestamp: env.ts,
        })
        break
      }
      case 'lock':
        // lock 走独立文件协议
        break
      case 'broadcast': {
        // 广播命令（如 reload-all）：通知本实例扩展重载
        log(`收到广播命令: ${env.command} (from ${env.from})`)
        if (env.command === 'reload' && latestCtx) {
          void pi.sendUserMessage('/__hub_reload', {
            deliverAs: 'steer',
            expandPromptTemplates: true,
          } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean }).catch(() => {})
        }
        break
      }
    }
  }

  async function handleTakeoverEnvelope(env: Extract<Envelope, { type: 'takeover' }>): Promise<void> {
    await handleTakeover({
      targetName: env.to,
      targetPid: 0,
      fromName: env.from,
      capability: env.capability || undefined,
      timestamp: env.ts,
    })
  }

  async function handleTakeover(req: TakeoverRequest): Promise<void> {
    if (req.capability === 'command') {
      const command = (req.payload as { command?: string } | undefined)?.command
      if (command) {
        log(`收到来自 ${req.fromName} 的指令: ${command}`)
        await pi.sendUserMessage(command, {
          deliverAs: 'steer',
          expandPromptTemplates: true,
        } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
      }
      return
    }
    log(`收到接管请求: ${req.targetName} (capability=${req.capability ?? 'default'})`)
    bridge.notifyTakeover(req)
    // 渠道让位回调：目标实例若正在轮询该 capability，应停止（由渠道注册）
    for (const cb of takeoverCallbacks.values()) {
      try {
        cb(req)
      } catch {
        // ignore
      }
    }
  }

  // --- 轮询：本地消息队列 ---

  async function pollMessages(): Promise<void> {
    try {
      // 服务器模式：消费本地队列（ack 语义）
      if (config.coordinatorPort && !config.coordinatorUrl) {
        const items = queue.dequeue(currentInstanceName)
        for (const { env, ack } of items) {
          try {
            routeEnvelope(env)
          } finally {
            ack()
          }
        }
      }
    } catch {
      // ignore
    }
  }

  // ============================================================================
  // 渠道入站处理（IGateway.onInbound → bridge → router）
  // ============================================================================

  async function handleCommand(text: string, userId: string, channel: string): Promise<string | null> {
    const ctx: CommandCtx = {
      currentInstanceName,
      collectInstances,
      resolveTarget,
      doSwitch,
      doSendCommand,
      doSendMessage,
      rememberTarget,
      getLastTarget: () => lastTargetName,
      doStartPi,
    }
    // 渠道正在等待问卷答案时禁用宽松 use 匹配（数字可能是答案，不是切换命令）
    const gw = gateways.get(channel)
    const awaitingAnswer = gw?.isAwaitingAnswer?.(userId) ?? false
    const result = await executeCommand(text, ctx, { loose: !awaitingAnswer })
    return result ? result.reply : null
  }

  async function handleMessage(m: InboundMessage): Promise<boolean> {
    void deliverToAgent(m)
    return true
  }

  async function deliverToAgent(m: InboundMessage): Promise<unknown> {
    // 协调消息直接注入（文本）
    if (m.channel === 'coord') {
      return pi.sendUserMessage(`[协调消息 @${m.userId}] ${m.text ?? ''}`, {
        deliverAs: 'steer',
        expandPromptTemplates: true,
      } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
    }
    // 普通渠道消息：带渠道标识注入
    return pi.sendUserMessage(m.text ?? '[消息]', {
      deliverAs: 'steer',
      expandPromptTemplates: true,
    } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
  }

  // ============================================================================
  // 工具：list_instances / switch_instance / send_command / send_message
  // ============================================================================

  pi.registerTool({
    name: 'list_instances',
    label: 'List Instances',
    description: '列出所有 pi 实例（本机 + 远程），标注当前实例。供实例间协调、切换前查看。',
    promptSnippet: '列出所有 pi 实例',
    promptGuidelines: ['用户询问实例状态、切换前先调用本工具。编号可用于 switch_instance 的 target。'],
    parameters: Type.Object({}),
    async execute() {
      try {
        const { local, all } = await collectInstances()
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
        return ok(await doSwitch(String(params.target).trim(), params.capability ?? 'wechat'))
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
        return ok(await doSendCommand(String(params.target).trim(), String(params.command).trim()))
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
        return ok(await doSendMessage(String(params.target).trim(), String(params.text).trim()))
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
        const { all } = await collectInstances()
        let inst: InstanceInfo | undefined
        if (params.target) {
          inst = resolveTarget(all, String(params.target).trim())
          if (!inst) return fail(`未找到实例 ${params.target}，先 /instances 查看`)
        } else {
          inst = all.find((i) => i.name === currentInstanceName)
          if (!inst) return fail('未找到当前实例')
        }
        return ok(await doStartPi(inst, params.cwd ? String(params.cwd) : undefined))
      } catch (err) {
        return fail(`启动失败: ${(err as Error).message}`)
      }
    },
  })

  // ============================================================================
  // 公共逻辑（工具与 / 命令共用）
  // ============================================================================

  async function collectInstances(): Promise<{ local: InstanceInfo[]; all: InstanceInfo[] }> {
    const remoteCfg = config.remoteHosts ?? {}
    const local = listInstances()
    const all: InstanceInfo[] = [...local]
    if (config.coordinatorPort) {
      // 协调中心模式：优先用实际活跃客户端（/inbox 轮询登记的实例名+主机名）；
      // remoteInstanceNames 作为已知但可能离线的补充
      const active = listActiveClients()
      const activeNames = new Set<string>()
      for (const name of active) {
        activeNames.add(name)
        if (!all.some((i) => i.name === name)) {
          all.push({ name, pid: 0, cwd: '', sessionId: '', lastSeen: 0, host: clientHostName(name) })
        }
      }
      for (const nm of config.remoteInstanceNames ?? []) {
        // 已被活跃客户端覆盖（实例名或主机名匹配）→ 跳过，避免重复条目
        const covered =
          activeNames.has(nm) ||
          all.some((i) => i.host === nm) ||
          active.some((name) => clientHostName(name) === nm)
        if (!covered) {
          all.push({ name: nm, pid: 0, cwd: '', sessionId: '', lastSeen: 0 })
        }
      }
    }
    if (config.coordinatorUrl) {
      const ci = await fetchCoordinatorInstances(config.coordinatorUrl)
      if (ci) {
        for (const li of ci.local) {
          if (!all.some((i) => i.name === li.name)) all.push({ name: li.name, pid: li.pid, cwd: li.cwd, sessionId: '', lastSeen: 0, host: li.host })
        }
        for (const c of ci.clients ?? []) {
          if (!all.some((i) => i.name === c.name)) all.push({ name: c.name, pid: 0, cwd: c.cwd ?? '', sessionId: '', lastSeen: 0, host: c.host })
        }
      }
    }
    try {
      const sshRemote = await listRemoteInstances(remoteCfg, path.join(STATE_DIR, 'instances.json'))
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

  let lastTargetName: string | null = null

  function rememberTarget(name: string): void {
    lastTargetName = name
  }

  async function doSwitch(name: string, capability?: string): Promise<string> {
    const { local, all } = await collectInstances()
    const target = resolveTarget(all, name)
    if (!target) return `未找到实例 ${name}，先 /instances 查看`
    const cap = capability ?? 'wechat'
    if (target.pid === process.pid) {
      const holder = getGlobalLockHolder()
      if (holder?.name === currentInstanceName) return `微信已由 ${target.name} 接管`
      if (config.coordinatorUrl) {
        await postEnvelope(config.coordinatorUrl, {
          type: 'takeover',
          id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
          from: currentInstanceName || 'local',
          to: target.name,
          capability: cap,
          ts: Date.now(),
        })
        rememberTarget(target.name)
        return `已请求 ${target.name} 接管微信`
      }
      if (config.coordinatorPort) {
        // 协调中心模式：本机即仲裁者，直接 force 抢锁并通知本机渠道接管
        const ok = coordinatorTryLock(currentInstanceName, process.pid, cap, true)
        if (!ok) return `接管 ${target.name} 失败：锁仍被 ${holder?.name ?? '未知'} 持有`
        rememberTarget(target.name)
        notifyAutoTakeover(cap)
        return `已请求 ${target.name} 接管微信（强制）`
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
    if (config.coordinatorPort && !local.some((i) => i.name === target.name)) {
      // 协调中心模式：目标是远程实例（活跃客户端或已知远程名）→ 服务器→局域网
      enqueueRemoteTakeover(req)
      return `已向实例 ${target.name} 发送接管请求`
    }
    if (config.coordinatorUrl && !local.some((i) => i.name === target.name)) {
      await postEnvelope(config.coordinatorUrl, {
        type: 'takeover',
        id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
        from: currentInstanceName || 'local',
        to: target.name,
        capability: cap,
        ts: Date.now(),
      })
      return `已向服务器实例 ${target.name} 发送接管请求`
    }
    const remoteTarget = config.remoteHosts?.[target.name]
    if (remoteTarget) {
      await writeRemoteTakeoverRequest(remoteTarget, req, TAKEOVER_FILE)
      return `已向远程实例 ${target.name} 发送接管请求`
    }
    return `实例 ${target.name} 没有可用的通道`
  }

  async function doSendCommand(target: string, command: string): Promise<string> {
    rememberTarget(target)
    const env: Envelope = {
      type: 'command',
      id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
      from: currentInstanceName || 'local',
      to: target,
      command,
      ts: Date.now(),
    }
    if (config.coordinatorPort && !listInstances().some((i) => i.name === target)) {
      // 协调中心模式：目标是远程实例 → 服务器→局域网
      enqueueRemoteTakeover({ targetName: target, targetPid: 0, fromName: env.from, capability: 'command', payload: { command }, timestamp: env.ts })
    } else if (config.coordinatorUrl) {
      await postEnvelope(config.coordinatorUrl, env)
    } else if (config.remoteHosts?.[target]) {
      await writeRemoteTakeoverRequest(config.remoteHosts[target], { targetName: target, targetPid: 0, fromName: env.from, capability: 'command', payload: { command }, timestamp: env.ts }, TAKEOVER_FILE)
    } else {
      writeLocalTakeoverRequest({ targetName: target, targetPid: 0, fromName: env.from, capability: 'command', payload: { command }, timestamp: env.ts })
    }
    return `已向实例 ${target} 发送指令: ${command}`
  }

  async function doSendMessage(target: string, text: string): Promise<string> {
    rememberTarget(target)
    const env: Envelope = {
      type: 'message',
      id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
      from: currentInstanceName || 'local',
      to: target,
      text,
      ts: Date.now(),
    }
    if (config.coordinatorPort && !config.coordinatorUrl) {
      // 服务器模式：本地入队（ack 语义消费）
      queue.enqueue(env)
    } else if (config.coordinatorUrl) {
      await postEnvelope(config.coordinatorUrl, env)
    } else {
      // 本机实例：直接写本地 takeover 通道？消息走本地队列
      queue.enqueue(env)
    }
    return `已向实例 ${target} 发送消息`
  }

  // ============================================================================
  // 启动 pi：在目标实例所在机器的 tmux 中新建会话运行 pi
  // ============================================================================

  function execCapture(file: string, args: string[], timeoutMs = 20000): Promise<string> {
    return new Promise((resolve) => {
      execFile(file, args, { timeout: timeoutMs, maxBuffer: 64 * 1024 }, (err, stdout) => {
        if (err) {
          resolve(`__EXEC_ERR__ ${(err as Error).message}`)
          return
        }
        resolve(stdout ?? '')
      })
    })
  }

  async function doStartPi(target: InstanceInfo, cwd?: string): Promise<string> {
    // 复用实例所在的 tmux 会话（不新建会话）：窗口名带唯一后缀（实例 pid，未知时回退时间戳）
    const pidSuffix = target.pid > 0 ? String(target.pid) : String(Date.now()).slice(-6)
    const winName = `pi-${target.name}-${pidSuffix}`
    const dir = cwd?.trim() || target.cwd?.trim() || '~'
    const isLocal = !target.host || target.host === os.hostname()
    const hostLabel = isLocal ? `${os.hostname()}（本机）` : target.host

    // 1) 定位当前 tmux 会话（复用，绝不新建）
    let sessionName: string
    let sessionTarget: string
    let remote: RemoteHostConfig | undefined
    if (!isLocal) remote = config.remoteHosts?.[target.name] ?? { target: target.host as string }
    try {
      if (isLocal) {
        if (!process.env.TMUX) return `❌ ${hostLabel} 当前实例不在 tmux 会话中，无法开窗口`
        sessionName = (await execCapture('tmux', ['display-message', '-p', '#S'])).trim()
        // 用 session id（如 $1）作 new-window 目标，避免纯数字会话名被当成窗口索引
        sessionTarget = (await execCapture('tmux', ['display-message', '-p', '#{session_id}'])).trim()
        if (!sessionName || !sessionTarget) return `❌ ${hostLabel} 无法获取当前 tmux 会话`
      } else {
        if (!target.pid || target.pid <= 0) return `❌ 实例 ${target.name} pid 未知，无法定位其 tmux 会话`
        const probe = [
          `SID=$(tr '\\0' '\\n' < /proc/${target.pid}/environ 2>/dev/null | sed -n 's/^TMUX=.*,\\([0-9][0-9]*\\)$/\\1/p' | head -1)`,
          `if [ -z "$SID" ]; then echo 'NO_TMUX'; exit 0; fi`,
          `tmux list-sessions -F '#{session_id} #{session_name}' | awk -v id="\\$$SID" '$1==id{print $1, $2; exit}'`,
        ].join('\n')
        const probeOut = (await sshExec(remote!.target, remote!.port, probe)).trim()
        const [idPart, namePart] = probeOut.split(/\s+/, 2)
        if (!idPart || idPart === 'NO_TMUX') return `❌ 实例 ${target.name} 不在 tmux 会话中（或会话已不存在）`
        sessionTarget = idPart
        sessionName = namePart || target.name
      }
    } catch (err) {
      return `❌ 启动失败（${hostLabel}）: ${(err as Error).message}`
    }

    // 2) 在复用的会话中开窗口
    const shellCmd = [
      `if tmux list-windows -t '${sessionTarget}' -F '#W' 2>/dev/null | grep -qx '${winName}'; then`,
      `  echo 'TMUX_EXISTS'`,
      `else`,
      `  tmux new-window -t '${sessionTarget}' -n '${winName}' -c '${dir}' "pi" && echo 'TMUX_STARTED' || echo 'TMUX_FAILED'`,
      `fi`,
    ].join('\n')

    let output: string
    try {
      if (isLocal) {
        output = await execCapture('bash', ['-lc', shellCmd])
      } else {
        output = await sshExec(remote!.target, remote!.port, shellCmd)
      }
    } catch (err) {
      return `❌ 启动失败（${hostLabel}）: ${(err as Error).message}`
    }

    const line = output.trim().split('\n').pop() ?? ''
    if (line.includes('TMUX_EXISTS')) {
      return `⚠️ 窗口 ${winName} 已存在（${hostLabel}，tmux 会话 ${sessionName}），未重复启动。查看: tmux attach -t ${sessionName}`
    }
    if (line.includes('TMUX_STARTED')) {
      return `✅ 已在 ${hostLabel} 启动 pi，tmux 会话 ${sessionName} 窗口 ${winName}（目录 ${dir}）。查看: tmux attach -t ${sessionName}`
    }
    return `❌ 启动失败（${hostLabel}）: ${output.trim() || 'tmux 执行异常'}`
  }

  // ============================================================================
  // 本地 takeover 文件（本机实例间）
  // ============================================================================

  function writeLocalTakeoverRequest(req: TakeoverRequest): void {
    try {
      fs.writeFileSync(TAKEOVER_FILE, JSON.stringify(req, null, 2), { mode: 0o600 })
    } catch {
      // ignore
    }
  }

  function readTakeoverRequest(): TakeoverRequest | null {
    try {
      const req = JSON.parse(fs.readFileSync(TAKEOVER_FILE, 'utf8')) as TakeoverRequest
      if (Date.now() - req.timestamp > TAKEOVER_TTL_MS) {
        clearTakeoverRequest()
        return null
      }
      return req
    } catch {
      return null
    }
  }

  function clearTakeoverRequest(): void {
    try {
      fs.unlinkSync(TAKEOVER_FILE)
    } catch {
      // ignore
    }
  }

  // ============================================================================
  // 广播（reload-all 等）：独立广播文件，多消费者各自 ack
  // ============================================================================

  interface BroadcastRequest {
    id: string
    command: string
    fromName: string
    timestamp: number
    acked: Record<string, boolean>
  }

  function writeBroadcastRequest(command: string): void {
    const req: BroadcastRequest = {
      id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
      command,
      fromName: currentInstanceName,
      timestamp: Date.now(),
      acked: {},
    }
    try {
      fs.writeFileSync(BROADCAST_FILE, JSON.stringify(req, null, 2), { mode: 0o600 })
    } catch {
      // ignore
    }
  }

  function readBroadcastRequest(): BroadcastRequest | null {
    try {
      return JSON.parse(fs.readFileSync(BROADCAST_FILE, 'utf8')) as BroadcastRequest
    } catch {
      return null
    }
  }

  function ackBroadcastRequest(): boolean {
    try {
      const req = readBroadcastRequest()
      if (!req) return false
      if (Date.now() - req.timestamp > TAKEOVER_TTL_MS) {
        fs.unlinkSync(BROADCAST_FILE)
        return false
      }
      if (req.acked[currentInstanceName]) return false
      req.acked[currentInstanceName] = true
      fs.writeFileSync(BROADCAST_FILE, JSON.stringify(req, null, 2), { mode: 0o600 })
      return true
    } catch {
      return false
    }
  }

  // ============================================================================
  // TUI 斜杠命令
  // ============================================================================

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
      if (target) notifyCmdResult(ctx, await doSendCommand(target, rest))
      else if (lastTargetName) notifyCmdResult(ctx, await doSendCommand(lastTargetName, rest))
      else ctx.ui.notify('未指定实例且没有上次实例，先 /use <实例> 或 /send-command <实例> <指令>', 'warning')
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
      if (target) notifyCmdResult(ctx, await doSendMessage(target, rest))
      else if (lastTargetName) notifyCmdResult(ctx, await doSendMessage(lastTargetName, rest))
      else ctx.ui.notify('未指定实例且没有上次实例，先 /use <实例> 或 /send-message <实例> <内容>', 'warning')
    },
  })

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

  // 内部命令：广播触发的扩展重载（broadcast envelope → 本命令）
  pi.registerCommand('__hub_reload', {
    description: '广播重载（内部命令，由 broadcast envelope 触发）',
    handler: async (_args, ctx) => {
      await ctx.reload()
    },
  })

  pi.registerCommand('start-pi', {
    getArgumentCompletions: instanceCompletions,
    description: '在实例（默认本机）的 tmux pi 会话中启动 pi：/start-pi [实例名] [目录]',
    handler: async (args, ctx) => {
      const parts = args.trim().split(/\s+/)
      const { all } = await collectInstances()
      let inst: InstanceInfo | undefined
      let rest = args.trim()
      if (parts[0] && all.some((i) => i.name === parts[0])) {
        inst = all.find((i) => i.name === parts[0])
        rest = parts.slice(1).join(' ')
      } else {
        // 未指定实例 → 默认当前实例（本机）
        inst = all.find((i) => i.name === currentInstanceName)
      }
      if (!inst) {
        ctx.ui.notify('未找到当前实例', 'warning')
        return
      }
      ctx.ui.notify(await doStartPi(inst, rest.trim() || undefined), 'info')
    },
  })

  async function parseTargetArgs(args: string): Promise<{ target: string | null; rest: string }> {
    const parts = args.trim().split(/\s+/)
    if (parts.length === 0) return { target: null, rest: '' }
    const { all } = await collectInstances()
    if (all.some((i) => i.name === parts[0])) {
      return { target: parts[0], rest: parts.slice(1).join(' ') }
    }
    return { target: null, rest: args.trim() }
  }

  // ============================================================================
  // 会话生命周期
  // ============================================================================

  pi.on('session_start', async (_event, ctx) => {
    latestCtx = ctx
    config = loadHubConfig()
    try {
      fs.appendFileSync('/tmp/pi-coordinator-msg.log', `[${new Date().toISOString()}] SESSION_START reason=${(_event as { reason?: string } | undefined)?.reason ?? '?'} cwd=${ctx.cwd} url=${config.coordinatorUrl ?? 'none'}\n`)
    } catch {
      // ignore
    }
    currentInstanceName = config.instanceName || path.basename(ctx.cwd) || 'pi'

    registerInstance({
      name: currentInstanceName,
      pid: process.pid,
      cwd: ctx.cwd,
      sessionId: ctx.sessionManager.getSessionId(),
      host: os.hostname(),
    })

    if (config.coordinatorPort && !g[watcherKey]) {
      const inUse = await isPortInUse(config.coordinatorPort)
      if (!inUse) {
        coordinatorServer = startCoordinatorServer(
          config.coordinatorPort,
          { name: currentInstanceName, pid: process.pid, cwd: ctx.cwd, host: os.hostname() },
          config.remoteInstanceNames ?? [],
          queue,
        )
        log(`协调中心已启动: 端口 ${config.coordinatorPort}`)
      } else {
        log(`协调端口 ${config.coordinatorPort} 已被占用，本实例作为客户端接入 (127.0.0.1:${config.coordinatorPort})`)
        config = { ...config, coordinatorUrl: `http://127.0.0.1:${config.coordinatorPort}` }
      }
    }

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

  // ============================================================================
  // globalThis 桥：渠道扩展通过桥接入（版本化，消除加载顺序问题）
  // ============================================================================

  g.__PI_HUB__ = {
    version: '2.0.0',
    registerGateway,
    onTakeoverRequest,
    getInstanceName: () => currentInstanceName || loadHubConfig().instanceName || path.basename(process.cwd()) || 'local',
    getCoordinatorUrl: () => config.coordinatorUrl,
    coordinatorTryLock,
    coordinatorReleaseLock,
    requestRemoteLock,
    releaseRemoteLock,
    getGlobalLockHolder,
    preassignLock,
    registerInstance,
    unregisterInstance,
    listInstances,
    getConfig: () => loadHubConfig(),
    /** 广播 reload-all：本机实例写广播文件 + 客户端模式 POST 到协调中心（跨机器分发） */
    broadcastReload: () => {
      writeBroadcastRequest('reload')
      if (config.coordinatorUrl) {
        void postEnvelope(config.coordinatorUrl, {
          type: 'broadcast',
          id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
          from: currentInstanceName || 'local',
          command: 'reload',
          ts: Date.now(),
        }).catch(() => {})
      }
    },
    /** 读取最后一条微信对话：协调中心模式读本地（权威），客户端模式从协调中心拉 */
    getLastMsg: async (): Promise<LastWechatMsg | null> => {
      if (config.coordinatorUrl) return fetchLastMsgRemote(config.coordinatorUrl)
      return readLastMsgLocal()
    },
    /** 写入最后一条微信对话：协调中心模式写本地（权威），客户端模式 POST 到协调中心 */
    setLastMsg: async (data: Partial<LastWechatMsg>): Promise<void> => {
      if (config.coordinatorUrl) {
        await pushLastMsgRemote(config.coordinatorUrl, data)
      } else {
        const prev = readLastMsgLocal()
        writeLastMsgLocal({
          userId: data.userId ?? prev?.userId ?? '',
          userMsg: (data.userMsg ?? prev?.userMsg ?? '').slice(0, 200),
          aiMsg: (data.aiMsg ?? prev?.aiMsg ?? '').slice(0, 500),
          ts: Date.now(),
        })
      }
    },
  }

  // 兼容旧桥（pi-wechat-assistant 旧版仍读 __PI_COORDINATOR__）
  g.__PI_COORDINATOR__ = {
    version: '0.1.0',
    coordinatorTryLock,
    coordinatorReleaseLock,
    registerInstance,
    unregisterInstance,
    listInstances,
    listActiveClients,
    getGlobalLockHolder,
    getConfig: () => loadHubConfig(),
  }

  // ============================================================================
  // 工具函数
  // ============================================================================

  function ok(text: string) {
    return { content: [{ type: 'text' as const, text }], details: {} }
  }

  function fail(text: string) {
    return { content: [{ type: 'text' as const, text: `❌ ${text}` }], details: {} }
  }

  const isDebug = !!process.env.PI_COORDINATOR_DEBUG
  function log(msg: string): void {
    if (isDebug) console.log(`[pi-hub] ${msg}`)
  }

  function isPortInUse(port: number): Promise<boolean> {
    return new Promise((resolve) => {
      const sock = net.connect(port, '127.0.0.1')
      sock.once('connect', () => { sock.destroy(); resolve(true) })
      sock.once('error', () => resolve(false))
    })
  }
}
