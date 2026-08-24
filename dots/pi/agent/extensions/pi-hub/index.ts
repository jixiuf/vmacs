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
import { registerInstance, unregisterInstance, listInstances, pruneInstances } from './src/registry.js'
import {
  coordinatorTryLock,
  coordinatorReleaseLock,
  getGlobalLockHolder,
  preassignLock,
} from './src/lock.js'
import { EnvelopeQueue } from './src/queue.js'
import {
  startCoordinatorServer,
  postEnvelope,
  fetchCoordinatorInstances,
  listRemoteInstances,
  sshExec,
  connectCoordinatorWS,
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
  type WsClientHandle,
  type LastWechatMsg,
  type RemoteHostConfig,
} from './src/transport.js'
import { SessionBridge } from './src/bridge.js'
import { Router } from './src/router.js'
import { executeCommand, toNumber, parseStartPiTarget, type CommandCtx } from './src/commands.js'
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
  let wsClient: WsClientHandle | null = null
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
  // reload 时关闭旧协调中心 server（旧代码无 WS upgrade），让新 session_start 用新代码重启
  const SERVER_KEY = '__PI_HUB_SERVER__'
  const oldServer = g[SERVER_KEY] as ReturnType<typeof startCoordinatorServer> | undefined
  if (oldServer) {
    try { oldServer.closeAllConnections?.() } catch { /* ignore */ }
    try { oldServer.close() } catch { /* ignore */ }
    delete g[SERVER_KEY]
  }
  g[watcherKey] = setInterval(() => {
    void pollIncoming().catch(() => {})
    void pollMessages().catch(() => {})
  }, 2000)
  g[`${watcherKey}_AUTO_TAKEOVER`] = setInterval(() => {
    void autoTakeoverIfIdle().catch(() => {})
    void ensureCoordinatorIfNeeded().catch(() => {})
  }, 5000)
  // 定期清理死亡实例条目（低频原子写，避免与注册/注销并发竞争）
  const PRUNE_KEY = '__PI_HUB_PRUNE__'
  if (!g[PRUNE_KEY]) {
    g[PRUNE_KEY] = setInterval(() => pruneInstances(), 60000)
  }

  // 兜底初始化：pi 重启恢复会话时扩展加载可能晚于 session_start 事件派发（事件错过），
  // 导致 currentInstanceName 未设置、实例不注册/不连 WS（哑巴实例）。
  // 扩展加载后延时检查，若 session_start 未触发则用 process.cwd() 兜底完成初始化。
  setTimeout(() => {
    if (currentInstanceName) return // session_start 已正常触发
    log(`session_start 未触发，兜底初始化（cwd=${process.cwd()}）`)
    try {
      currentInstanceName = registerInstance({
        name: process.env.PI_INSTANCE_NAME || config.instanceName || path.basename(process.cwd()) || 'pi',
        pid: process.pid,
        cwd: process.cwd(),
        sessionId: 'fallback',
        host: os.hostname(),
        sessionName: pi.getSessionName() ?? undefined,
      })
      if (config.coordinatorUrl) {
        wsClient?.close()
        wsClient = connectCoordinatorWS(
          config.coordinatorUrl,
          currentInstanceName,
          os.hostname(),
          (env) => routeEnvelope(env),
          (connected) => {
            log(connected ? '协调中心 WS 已连接（兜底初始化）' : '协调中心 WS 断开，重连中')
            if (connected) flushPendingOutbox()
          },
          pi.getSessionName() ?? undefined,
        )
      }
    } catch (err) {
      log(`兜底初始化失败: ${(err as Error).message}`)
    }
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
      if (config.coordinatorPort) {
        // 协调中心模式：消费本地队列中发给自己的 envelope（指令/消息），ack 防重投
        const items = queue.dequeue(currentInstanceName)
        for (const item of items) {
          routeEnvelope(item.env)
          item.ack()
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
      safeSendUserMessage('/__hub_reload', {
        deliverAs: 'steer',
        expandPromptTemplates: true,
      } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
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
        safeSendUserMessage(env.command, {
          deliverAs: 'steer',
          expandPromptTemplates: true,
        } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
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
          safeSendUserMessage('/__hub_reload', {
            deliverAs: 'steer',
            expandPromptTemplates: true,
          } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
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
        safeSendUserMessage(command, {
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
      doReloadAll,
      writeClipboard,
    }
    // 渠道正在等待问卷答案时禁用宽松 use 匹配（数字可能是答案，不是切换命令）
    const gw = gateways.get(channel)
    const awaitingAnswer = gw?.isAwaitingAnswer?.(userId) ?? false
    const result = await executeCommand(text, ctx, { loose: !awaitingAnswer })
    return result ? result.reply : null
  }

  /**
   * 安全投递消息到 agent 会话：扩展 reload / 会话替换后，旧扩展实例的 pi API 会同步抛
   * "ctx is stale" 错误（uncaughtException 会让整个 pi 进程退出）。所有 sendUserMessage
   * 调用都必须经此包装捕获，stale 时静默丢弃。
   */
  function safeSendUserMessage(
    content: string,
    opts?: Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates?: boolean },
  ): void {
    try {
      const p = pi.sendUserMessage(content, opts)
      if (p && typeof (p as Promise<unknown>).catch === 'function') {
        void (p as Promise<unknown>).catch(() => { /* 异步失败忽略 */ })
      }
    } catch (err) {
      log(`sendUserMessage 失败（扩展可能已 reload，消息丢弃）: ${(err as Error).message}`)
    }
  }

  async function handleMessage(m: InboundMessage): Promise<boolean> {
    void deliverToAgent(m)
    return true
  }

  async function deliverToAgent(m: InboundMessage): Promise<unknown> {
    // 协调消息直接注入（文本）
    if (m.channel === 'coord') {
      safeSendUserMessage(`[协调消息 @${m.userId}] ${m.text ?? ''}`, {
        deliverAs: 'steer',
        expandPromptTemplates: true,
      } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
      return null
    }
    // 普通渠道消息：带渠道标识注入
    safeSendUserMessage(m.text ?? '[消息]', {
      deliverAs: 'steer',
      expandPromptTemplates: true,
    } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
    return null
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
          const reply = await doSendMessage(t.instance, `${tag} ${t.task}\n完成后请回复「${tag}结果」+ 结果内容。`)
          lines.push(`TASK#${i + 1} → ${t.instance}: ${reply}`)
        }
        return ok(`已分发 ${params.tasks.length} 个任务：\n${lines.join('\n')}\n\n等待各实例回传「[TASK#N结果]」，收到后请汇总。`)
      } catch (err) {
        return fail(`分发失败: ${(err as Error).message}`)
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
    // 支持 name 与 name@host 两种形式（list_instances 展示的是 name@host）
    return (
      all.find((i) => i.name === name) ??
      all.find((i) => i.host && `${i.name}@${i.host}` === name)
    )
  }

  const instanceCompletions = async (argumentPrefix: string) => {
    const { local, all } = await collectInstances()
    return all
      .filter((i) => i.name.startsWith(argumentPrefix))
      .map((i) => {
        const isRemote = !local.some((l) => l.name === i.name)
        const isCurrent = i.name === currentInstanceName || i.pid === process.pid
        const marks: string[] = []
        if (isCurrent) marks.push('当前')
        if (isRemote) marks.push('远程')
        const mark = marks.length > 0 ? `（${marks.join('，')}）` : ''
        const label = `${i.host ? `${i.name}@${i.host}` : i.name}${mark}`
        const title = i.sessionName ? ` 「${i.sessionName}」` : ''
        const description = `${i.host ? `@${i.host} · ${i.cwd || '(远程)'}` : i.cwd}${title}`
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
        // 目标为当前实例：不能只发 takeover envelope——sendEnvelopeToCoordinator 的
        // 自投防护（isCurrentInstance）会直接拦截，切换静默失效。改为经协调中心
        // force 抢锁 + 通知本机渠道接管（与协调中心模式分支同语义）。
        try {
          await requestRemoteLock(config.coordinatorUrl, currentInstanceName, process.pid, cap, true)
        } catch {
          // ignore
        }
        rememberTarget(target.name)
        notifyAutoTakeover(cap)
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
      await sendEnvelopeToCoordinator({
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

  /** 重载所有实例：当前实例本地执行，其他实例发 /__hub_reload（扩展命令，注入后目标端真正执行 reload；内置 /reload 注入只显示文本不执行） */
  async function doReloadAll(): Promise<string> {
    const { all } = await collectInstances()
    if (all.length === 0) return '没有登记的实例'
    const lines: string[] = []
    for (const inst of all) {
      lines.push(await doSendCommand(inst.name, '/__hub_reload'))
    }
    return `已向 ${all.length} 个实例发送 reload:\n${lines.join('\n')}`
  }

  /** 重载当前实例扩展：仅命令上下文（ExtensionCommandContext）有 reload()，session 上下文没有 */
  function reloadCurrentInstance(): boolean {
    const ctx = latestCtx as { reload?: () => unknown } | null
    if (ctx && typeof ctx.reload === 'function') {
      void ctx.reload()
      return true
    }
    return false
  }

  /** 发 envelope 到协调中心：WS 在线 → 直接发送（低延迟）；否则回退 HTTP POST */
  async function sendEnvelopeToCoordinator(env: Envelope): Promise<void> {
    // 自投防护：目标为当前实例时不发协调中心（避免回投注入形成消息循环）
    const to = (env as { to?: string }).to
    if (to && isCurrentInstance(to)) {
      if (env.type === 'message') {
        safeSendUserMessage(`[本地消息] ${(env as { text?: string }).text ?? ''}`, {
          deliverAs: 'steer',
          expandPromptTemplates: true,
        } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
      }
      return
    }
    if (wsClient && wsClient.sendEnvelope(env)) return
    if (config.coordinatorUrl) {
      try {
        await postEnvelope(config.coordinatorUrl, env)
        return
      } catch { /* 协调中心不可达 → 降级本地排队 */ }
    }
    // 降级：本地暂存，WS 恢复后 flush 补发（消息不丢）
    pendingOutbox.push(env)
    if (pendingOutbox.length > 200) pendingOutbox.shift()
  }

  /** 协调中心不可达时的发送降级队列（WS 恢复后补发） */
  const pendingOutbox: Envelope[] = []

  /** WS 恢复（onStatus=true）时补发积压消息 */
  function flushPendingOutbox(): void {
    if (!wsClient || pendingOutbox.length === 0) return
    const still: Envelope[] = []
    for (const env of pendingOutbox) {
      if (!wsClient.sendEnvelope(env)) still.push(env)
    }
    pendingOutbox.length = 0
    for (const env of still) pendingOutbox.push(env)
    if (still.length > 0) log(`仍有 ${still.length} 条消息待补发（协调中心未完全恢复）`)
  }

  /** 目标是否是当前实例（名字相同或注册 pid 相同） */
  function isCurrentInstance(name: string): boolean {
    return name === currentInstanceName || (listInstances().find((i) => i.name === name)?.pid ?? 0) === process.pid
  }

  async function doSendCommand(target: string, command: string): Promise<string> {
    rememberTarget(target)
    // 归一化目标：name@host / 编号 → 实例名（协调中心按实例名匹配 inbox）
    const { all } = await collectInstances()
    const resolved = resolveTarget(all, target)
    const realTarget = resolved?.name ?? target
    // 目标为当前实例：本地直接执行（/reload → 扩展重载），
    // 避免经协调中心回投注入会话，形成“发送→注入→再发送”的自我循环
    if (isCurrentInstance(realTarget)) {
      const cmd = command.trim().toLowerCase()
      if (cmd === '/reload' || cmd === 'reload' || cmd === '/__hub_reload') {
        return reloadCurrentInstance()
          ? `已向实例 ${target} 发送指令: ${command}（当前实例，已本地执行 reload）`
          : `❌ 当前实例上下文不支持 reload()，请直接在 TUI 执行 /reload`
      }
      return `❌ 目标 ${target} 是当前实例，仅支持本地执行 /reload（其他指令请在目标实例会话中操作）`
    }
    // 远程实例：envelope 注入（__hub_cmd 包装，pi 命令系统执行）
    // 不依赖 tmux 模拟输入（TUI 忙碌时不生效，不可靠）
    if (resolved) {
      return await doSendCommandViaEnvelope(realTarget, command)
    }
    return `❌ 目标 ${target} 无法投递指令`
  }

  /** 远程指令：envelope 注入（/ 指令包装为 __hub_cmd 由目标 pi 命令系统执行） */
  async function doSendCommandViaEnvelope(target: string, command: string): Promise<string> {
    const env: Envelope = {
      type: 'command',
      id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
      from: currentInstanceName || 'local',
      to: target,
      // 远程执行：/ 开头的指令包装为 __hub_cmd（目标实例注入后触发 handler 真正执行）；
      // __hub_* 内部命令保持原样；非 / 文本原样注入（普通提示）
      command: command.startsWith('/') && !command.startsWith('/__hub_')
        ? `/__hub_cmd ${command}`
        : command,
      ts: Date.now(),
    }
    if (config.coordinatorPort && !listInstances().some((i) => i.name === target)) {
      // 协调中心模式：目标是远程实例 → 服务器→局域网
      enqueueRemoteTakeover({ targetName: target, targetPid: 0, fromName: env.from, capability: 'command', payload: { command }, timestamp: env.ts })
      return `已向实例 ${target} 发送指令: ${command}`
    } else if (config.coordinatorUrl) {
      try {
        await sendEnvelopeToCoordinator(env)
        return `已向实例 ${target} 发送指令: ${command}（注入）`
      } catch (err) {
        return `❌ 指令投递失败（${target}）: ${(err as Error).message}`
      }
    } else if (config.remoteHosts?.[target]) {
      await writeRemoteTakeoverRequest(config.remoteHosts[target], { targetName: target, targetPid: 0, fromName: env.from, capability: 'command', payload: { command }, timestamp: env.ts }, TAKEOVER_FILE)
      return `已向实例 ${target} 发送指令: ${command}`
    } else {
      writeLocalTakeoverRequest({ targetName: target, targetPid: 0, fromName: env.from, capability: 'command', payload: { command }, timestamp: env.ts })
      return `已向实例 ${target} 发送指令: ${command}`
    }
  }

  async function doSendMessage(target: string, text: string): Promise<string> {
    rememberTarget(target)
    // 归一化目标：name@host / 编号 → 实例名
    const { all } = await collectInstances()
    const resolved = resolveTarget(all, target)
    const realTarget = resolved?.name ?? target
    // 目标为当前实例：本地注入一次（带标记，不经过协调中心），避免自我循环
    // 注意用归一化后的 realTarget（name@host / 编号也能正确命中自己）
    if (isCurrentInstance(realTarget)) {
      safeSendUserMessage(`[本地消息] ${text}`, {
        deliverAs: 'steer',
        expandPromptTemplates: true,
      } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
      return `已向实例 ${target} 发送消息（当前实例，本地注入）`
    }
    const env: Envelope = {
      type: 'message',
      id: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
      from: currentInstanceName || 'local',
      to: realTarget,
      text,
      ts: Date.now(),
    }
    if (config.coordinatorPort && !config.coordinatorUrl) {
      // 服务器模式：本地入队（ack 语义消费）
      queue.enqueue(env)
    } else if (config.coordinatorUrl) {
      await sendEnvelopeToCoordinator(env)
    } else {
      // 本机实例：直接写本地 takeover 通道？消息走本地队列
      queue.enqueue(env)
    }
    return `已向实例 ${target} 发送消息`
  }

  // ============================================================================
  // 启动 pi：在目标实例所在机器的 tmux 中新建会话运行 pi
  // ============================================================================

  function execCapture(file: string, args: string[], timeoutMs = 20000): Promise<{ ok: boolean; stdout: string; stderr: string }> {
    return new Promise((resolve) => {
      execFile(file, args, { timeout: timeoutMs, maxBuffer: 64 * 1024 }, (err, stdout, stderr) => {
        if (err) {
          resolve({ ok: false, stdout: stdout ?? '', stderr: stderr?.toString() ?? (err as Error).message })
          return
        }
        resolve({ ok: true, stdout: stdout ?? '', stderr: stderr?.toString() ?? '' })
      })
    })
  }

  /** 写入系统剪贴板：macOS pbcopy / Linux xclip / Windows clip */
  function writeClipboard(text: string): Promise<boolean> {
    return new Promise((resolve) => {
      const cmd =
        process.platform === 'darwin' ? 'pbcopy'
        : process.platform === 'linux' ? (process.env.DISPLAY ? 'xclip' : 'xclip')
        : process.platform === 'win32' ? 'clip' : null
      if (!cmd) { resolve(false); return }
      try {
        const child = execFile(cmd, process.platform === 'linux' ? ['-selection', 'clipboard'] : [], (err) => resolve(!err))
        child.stdin?.write(text)
        child.stdin?.end()
      } catch {
        resolve(false)
      }
    })
  }

  /** 目录/路径安全校验：拒绝 shell 元字符，杜绝注入（远程经 shell 执行） */
  function isSafePath(p: string): boolean {
    return !/[;&|`'"$()<>*?\[\]{}#\\\n\r]/.test(p)
  }

  async function doStartPi(target: InstanceInfo, cwd?: string): Promise<string> {
    // 参数安全校验：实例名只允许安全字符，目录拒绝 shell 元字符（远程=shell 执行，必须防注入）
    if (!/^[A-Za-z0-9._-]+$/.test(target.name)) {
      return `❌ 实例名 ${target.name} 含不安全字符，拒绝启动`
    }
    const dir = cwd?.trim() || target.cwd?.trim() || '~'
    if (!isSafePath(dir)) {
      return `❌ 启动目录含不安全字符，拒绝启动: ${dir}`
    }
    // 复用实例所在的 tmux 会话（不新建会话）：窗口名带唯一后缀（实例 pid，未知时回退时间戳）
    const pidSuffix = target.pid > 0 ? String(target.pid) : String(Date.now()).slice(-6)
    const winName = `pi-${target.name}-${pidSuffix}`
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
        sessionName = (await execCapture('tmux', ['display-message', '-p', '#S'])).stdout.trim()
        // 用 session id（如 $1）作 new-window 目标，避免纯数字会话名被当成窗口索引
        sessionTarget = (await execCapture('tmux', ['display-message', '-p', '#{session_id}'])).stdout.trim()
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
    let status: string
    try {
      if (isLocal) {
        // 本机：execFile 数组参数（不经 shell），窗口去重后创建
        const listRes = await execCapture('tmux', ['list-windows', '-t', sessionTarget, '-F', '#W'])
        if (listRes.ok && listRes.stdout.split('\n').some((w) => w.trim() === winName)) {
          status = 'TMUX_EXISTS'
        } else {
          const createRes = await execCapture('tmux', ['new-window', '-t', sessionTarget, '-n', winName, '-c', dir, 'pi'])
          status = createRes.ok ? 'TMUX_STARTED' : `TMUX_FAILED ${createRes.stderr.trim()}`
        }
      } else {
        const shellCmd = [
          `if tmux list-windows -t '${sessionTarget}' -F '#W' 2>/dev/null | grep -qx '${winName}'; then`,
          `  echo 'TMUX_EXISTS'`,
          `else`,
          `  tmux new-window -t '${sessionTarget}' -n '${winName}' -c '${dir}' "pi" && echo 'TMUX_STARTED' || echo 'TMUX_FAILED'`,
          `fi`,
        ].join('\n')
        status = (await sshExec(remote!.target, remote!.port, shellCmd)).trim().split('\n').pop() ?? ''
      }
    } catch (err) {
      return `❌ 启动失败（${hostLabel}）: ${(err as Error).message}`
    }

    if (status.includes('TMUX_EXISTS')) {
      return `⚠️ 窗口 ${winName} 已存在（${hostLabel}，tmux 会话 ${sessionName}），未重复启动。查看: tmux attach -t ${sessionName}`
    }
    if (status.includes('TMUX_STARTED')) {
      return `✅ 已在 ${hostLabel} 启动 pi，tmux 会话 ${sessionName} 窗口 ${winName}（目录 ${dir}）。查看: tmux attach -t ${sessionName}`
    }
    return `❌ 启动失败（${hostLabel}）: ${status.replace('TMUX_FAILED', '').trim() || 'tmux 执行异常'}`
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
        const host = inst.host ? `@${inst.host}` : ''
        // cwd 列在行尾（远程实例 cwd 可能为空则不显示）
        const cwd = inst.cwd ? ` ${inst.cwd}` : ''
        return `${inst.name}${host}${mark}${cwd}`
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

  // 内部命令：远程命令执行（command envelope 注入 /__hub_cmd <cmd> 触发，真正在目标实例执行）
  pi.registerCommand('__hub_cmd', {
    description: '内部命令：远程执行会话命令（/new /fork /goto /reload /name 等，未知命令回退 tmux 模拟输入）',
    handler: async (args, ctx) => {
      const parts = args.trim().split(/\s+/)
      const raw = parts[0] ?? ''
      const rest = parts.slice(1).join(' ')
      const cmd = raw.replace(/^\//, '')
      try {
        switch (cmd) {
          case 'new':
            log(`[HUB-CMD] 远程执行 /new`)
            await ctx.newSession()
            break
          case 'fork':
            if (rest) {
              log(`[HUB-CMD] 远程执行 /fork ${rest}`)
              await ctx.fork(rest)
            }
            break
          case 'goto':
          case 'switch':
            if (rest) {
              log(`[HUB-CMD] 远程执行 /goto ${rest}`)
              await ctx.switchSession(rest)
            }
            break
          case 'reload':
            log(`[HUB-CMD] 远程执行 /reload`)
            await ctx.reload()
            break
          case 'name':
            if (rest) {
              log(`[HUB-CMD] 远程执行 /name ${rest}`)
              pi.setSessionName(rest)
            }
            break
          case 'thinking':
            if (rest) {
              log(`[HUB-CMD] 远程执行 /thinking ${rest}`)
              pi.setThinkingLevel(rest as Parameters<typeof pi.setThinkingLevel>[0])
            }
            break
          case 'quit':
            // 退出 pi：命令上下文无 quit API，直接退出进程（不依赖 tmux）；注册条目由定时 prune 清理
            log(`[HUB-CMD] 远程执行 /quit（退出 pi）`)
            setTimeout(() => process.exit(0), 300)
            break
          default:
            // 未知命令：仅记录，不执行（避免不可靠的 tmux 模拟输入）
            log(`[HUB-CMD] 未知命令: ${raw}，忽略`)
        }
      } catch (err) {
        log(`[HUB-CMD] 执行 ${raw} 失败: ${(err as Error).message}`)
      }
    },
  })

  pi.registerCommand('reloadall', {
    description: '重载所有实例（当前实例本地执行，其他实例发指令）',
    handler: async (_args, ctx) => {
      const { all } = await collectInstances()
      if (all.length === 0) {
        ctx.ui.notify('没有登记的实例', 'warning')
        return
      }
      const lines: string[] = []
      let needLocalReload = false
      for (const inst of all) {
        if (inst.pid === process.pid || inst.name === currentInstanceName) {
          needLocalReload = true
          lines.push(`${inst.name}: 将本地 reload`)
        } else {
          lines.push(await doSendCommand(inst.name, '/__hub_reload'))
        }
      }
      // 注意：ctx.reload() 后 ctx 即失效（stale），必须先 notify 再 reload，reload 放最后
      ctx.ui.notify(lines.join('\n'), 'info')
      if (needLocalReload) {
        await ctx.reload()
      }
    },
  })

  pi.registerCommand('start-pi', {
    getArgumentCompletions: instanceCompletions,
    description: '在实例（默认本机）的 tmux pi 会话中启动 pi：/start-pi [实例名] [目录]',
    handler: async (args, ctx) => {
      const { all } = await collectInstances()
      const parsed = parseStartPiTarget(all, currentInstanceName, args)
      if ('error' in parsed) {
        ctx.ui.notify(parsed.error, 'warning')
        return
      }
      ctx.ui.notify(await doStartPi(parsed.inst, parsed.rest.trim() || undefined), 'info')
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
    // 实例名优先级：PI_INSTANCE_NAME 环境变量（远程 subagent 用）> config > cwd basename
    currentInstanceName =
      process.env.PI_INSTANCE_NAME || config.instanceName || path.basename(ctx.cwd) || 'pi'
    // 先注册（同名存活实例会自动改名，保证唯一），再连 WS——WS 必须用改名后的名字，
    // 否则多个同 cwd 实例会以同名连接协调中心，wsClients 互相覆盖导致收不到消息
    currentInstanceName = registerInstance({
      name: currentInstanceName,
      pid: process.pid,
      cwd: ctx.cwd,
      sessionId: ctx.sessionManager.getSessionId(),
      host: os.hostname(),
      sessionName: pi.getSessionName() ?? undefined,
    })

    // 客户端模式：WS 长连接接收协调中心推送（替代 2s HTTP 轮询，低延迟）
    if (config.coordinatorUrl) {
      wsClient?.close()
      wsClient = connectCoordinatorWS(
        config.coordinatorUrl,
        currentInstanceName,
        os.hostname(),
        (env) => routeEnvelope(env),
        (connected) => {
          log(connected ? '协调中心 WS 已连接' : '协调中心 WS 断开，重连中')
          if (connected) flushPendingOutbox()
        },
        pi.getSessionName() ?? undefined,
      )
    }

    if (config.coordinatorPort && !coordinatorServer) {
      const inUse = await isPortInUse(config.coordinatorPort)
      if (!inUse) {
        coordinatorServer = startCoordinatorServer(
          config.coordinatorPort,
          { name: currentInstanceName, pid: process.pid, cwd: ctx.cwd, host: os.hostname() },
          config.remoteInstanceNames ?? [],
          queue,
        )
        g['__PI_HUB_SERVER__'] = coordinatorServer
        // listen 失败（端口实际被占，isPortInUse 误判）→ 降级为客户端，避免卡在协调中心模式死锁
        coordinatorServer.on('error', (err: NodeJS.ErrnoException) => {
          if (err.code === 'EADDRINUSE') {
            log(`协调端口 ${config.coordinatorPort} 被占用，降级为客户端接入`)
            config = { ...config, coordinatorUrl: `http://127.0.0.1:${config.coordinatorPort}` }
          }
        })
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
        void sendEnvelopeToCoordinator({
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
