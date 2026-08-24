// ============================================================================
// pi-hub — pi session 间协调核心
// 实例注册、接管切换、指令/消息互发、广播；IM 渠道通过 IGateway 接入。
// 由 pi-coordinator 演化：协调逻辑收敛于此，渠道退化为纯协议网关。
// ============================================================================

import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import * as net from 'node:net'

try {
  logEvent('EXT_LOADED', `pid=${process.pid}`)
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
import { executeCommand, toNumber, type CommandCtx } from './src/commands.js'
import { doStartPi as doStartPiFn, writeClipboard, type StartPiDeps } from './src/start-pi.js'
import { registerTools } from './src/tools.js'
import { log, logEvent } from './src/logger.js'
import { TaskRegistry } from './src/task.js'
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

  // start-pi 依赖（remoteHosts 取自 config，sshExec 来自传输层）
  const startPiDeps: StartPiDeps = {
    remoteHosts: config.remoteHosts ?? {},
    sshExec,
  }

  const queue = new EnvelopeQueue()
  // subagent 任务注册表（tasks.json 持久化）
  const taskRegistry = new TaskRegistry()
  const bridge = new SessionBridge({ pi, deliverToAgent: deliverToAgent })
  const router = new Router({
    handleCommand: handleCommand,
    handleMessage: handleMessage,
    getGateway: (channel) => gateways.get(channel),
    // envelope 分发实现：装配方提供各类型处理（实现留在 index.ts，分发收敛于 Router）
    onMessage: (env) => {
      log(`收到来自 ${env.from} 的消息: ${env.text.slice(0, 50)}`)
      logEvent('RECV', `from=${env.from} text=${env.text.slice(0, 80)} id=${env.id}`)
      // 识别 subagent 任务回传（[TASK-xxx结果] / 含任务 ID 的正文）→ 自动更新注册表
      tryAutoUpdateTask(env.from, env.text)
      bridge.handleInbound({
        id: env.id,
        channel: 'coord',
        userId: env.from,
        text: env.text,
        ts: env.ts,
      })
    },
    onCommand: (env) => {
      log(`收到来自 ${env.from} 的指令: ${env.command}`)
      safeSendUserMessage(env.command, {
        deliverAs: 'steer',
        expandPromptTemplates: true,
      } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
    },
    onTakeover: (env) => {
      handleTakeover({
        targetName: env.to,
        targetPid: 0,
        fromName: env.from,
        capability: env.capability,
        timestamp: env.ts,
      })
    },
    onBroadcast: (env) => {
      // 广播命令（如 reload-all）：通知本实例扩展重载
      log(`收到广播命令: ${env.command} (from ${env.from})`)
      if (env.command === 'reload' && latestCtx) {
        safeSendUserMessage('/__hub_reload', {
          deliverAs: 'steer',
          expandPromptTemplates: true,
        } as Parameters<typeof pi.sendUserMessage>[1] & { expandPromptTemplates: boolean })
      }
    },
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
  // subagent 任务监控：30s 超时/重试/自动回收（幂等，重复加载不叠加）
  const TASK_KEY = '__PI_HUB_TASK_MONITOR__'
  if (!g[TASK_KEY]) {
    g[TASK_KEY] = setInterval(() => void taskMonitorTick().catch(() => {}), 30_000)
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
    logEvent('WATCHER_STARTED', `pid=${process.pid}`)
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
    } catch (err) {
      log(`pollIncoming 异常: ${(err as Error).message}`)
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

  /** envelope 入站（统一经 Router 分发） */
  function routeEnvelope(env: Envelope): void {
    router.routeEnvelope(env)
  }

  /**
   * 识别 subagent 任务回传并自动更新注册表。
   * 协议：子实例回传文本含任务 ID（TASK-\d+-[a-z0-9]+），发送者是任务的 assignee，
   * 且任务未终结 → 提取正文为 result，按 JSON status 判定 done/failed。
   */
  function tryAutoUpdateTask(from: string, text: string): void {
    const m = text.match(/TASK-\d+-[a-z0-9]+/)
    if (!m) return
    const t = taskRegistry.get(m[0])
    if (!t || t.assignee !== from) return
    if (t.status === 'done' || t.status === 'failed' || t.status === 'timeout') return
    // 去前缀（[TASK-xxx结果] / [TASK#N结果]）与任务 ID，取剩余正文
    const result = text
      .replace(/\[?TASK-\d+-[a-z0-9]+\s*结果?\]?/g, '')
      .replace(/\[TASK#\d+\s*结果?\]?/g, '')
      .trim()
    const isFail = /"status"\s*:\s*"failed"/.test(result) || /失败|出错|error/i.test(result.slice(0, 80))
    taskRegistry.update(t.id, { status: isFail ? 'failed' : 'done', result })
    log(`任务 ${t.id} 收到回传：${isFail ? 'failed' : 'done'}`)
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
    } catch (err) {
      log(`pollMessages 异常: ${(err as Error).message}`)
    }
  }

  // ============================================================================
  // 渠道入站处理（IGateway.onInbound → bridge → router）
  // ============================================================================

  /** 构造命令上下文（TUI 命令与渠道命令共用同一实现） */
  function buildCommandCtx(): CommandCtx {
    return {
      currentInstanceName,
      collectInstances,
      resolveTarget,
      doSwitch,
      doSendCommand,
      doSendMessage,
      rememberTarget,
      getLastTarget: () => lastTargetName,
      doStartPi: (target, cwd) => doStartPiFn(target, cwd, startPiDeps),
      doReloadAll,
      writeClipboard,
      taskRegistry,
    }
  }

  async function handleCommand(text: string, userId: string, channel: string): Promise<string | null> {
    const ctx = buildCommandCtx()
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
  // 工具注册（list_instances / switch_instance / send_command / send_message / start_pi / dispatch_task）
  // ============================================================================

  registerTools(pi, {
    currentInstanceName: () => currentInstanceName,
    collectInstances,
    resolveTarget,
    doSwitch,
    doSendCommand,
    doSendMessage,
    doStartPi: (target, cwd) => doStartPiFn(target, cwd, startPiDeps),
    taskRegistry,
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
  // subagent 任务监控：30s 超时/重试/自动回收（由 TASK_KEY 定时器驱动）
  // ============================================================================

  async function taskMonitorTick(): Promise<void> {
    try {
      const events = taskRegistry.monitor()
      for (const ev of events) {
        if (ev.kind === 'retry' && ev.id && ev.assignee) {
          const t = taskRegistry.get(ev.id)
          if (!t) continue
          const tag = `[TASK#${ev.id.slice(-3)}]`
          log(`任务 ${ev.id} 超时重试（attempts=${t.attempts}）`)
          await doSendMessage(ev.assignee, `${tag}（超时重试）${t.payload}\n完成后请回复「${tag}结果」+ 内容，任务ID: ${ev.id}`)
        } else if (ev.kind === 'timeout') {
          log(`任务 ${ev.id} 超时放弃`)
        } else if (ev.kind === 'reclaim' && ev.assignee) {
          // 保护：不回收协调中心实例（/quit 会中断全部客户端）
          if (config.coordinatorPort && !config.coordinatorUrl && ev.assignee === currentInstanceName) {
            log(`跳过回收协调中心实例 ${ev.assignee}`)
            continue
          }
          log(`自动回收子实例 ${ev.assignee}`)
          await doSendCommand(ev.assignee, '/quit')
        }
      }
    } catch (err) {
      log(`taskMonitor 异常: ${(err as Error).message}`)
    }
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
  // ============================================================================
  // TUI 斜杠命令：统一复用 commands.ts 命令表
  // （/instances /use /send-command /send-message /cmd /msg /start-pi /reloadall /clipboard）
  // ============================================================================

  const TUI_COMMAND_MAP: Record<string, string> = {
    instances: 'instances',
    use: 'use',
    'send-command': 'cmd',
    'send-message': 'msg',
    cmd: 'cmd',
    msg: 'msg',
    'start-pi': 'start-pi',
    reloadall: 'reloadall',
    clipboard: 'clipboard',
  }
  for (const [tuiName, cmdName] of Object.entries(TUI_COMMAND_MAP)) {
    pi.registerCommand(tuiName, {
      getArgumentCompletions: instanceCompletions,
      description: `/${tuiName}（复用渠道命令实现）`,
      handler: async (args, ctx) => {
        try {
          const result = await executeCommand(`/${cmdName}${args.trim() ? ' ' + args.trim() : ''}`, buildCommandCtx(), { loose: false })
          if (result?.reply) ctx.ui.notify(result.reply, 'info')
        } catch (err) {
          ctx.ui.notify(`命令失败: ${(err as Error).message}`, 'error')
        }
      },
    })
  }

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
    logEvent('SESSION_START', `reason=${(_event as { reason?: string } | undefined)?.reason ?? '?'} cwd=${ctx.cwd} url=${config.coordinatorUrl ?? 'none'}`)
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
    const taskMonitor = g[TASK_KEY] as ReturnType<typeof setInterval> | undefined
    if (taskMonitor) {
      clearInterval(taskMonitor)
      delete g[TASK_KEY]
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

  // ============================================================================
  // 工具函数
  // ============================================================================

  function ok(text: string) {
    return { content: [{ type: 'text' as const, text }], details: {} }
  }

  function fail(text: string) {
    return { content: [{ type: 'text' as const, text: `❌ ${text}` }], details: {} }
  }

  function isPortInUse(port: number): Promise<boolean> {
    return new Promise((resolve) => {
      const sock = net.connect(port, '127.0.0.1')
      sock.once('connect', () => { sock.destroy(); resolve(true) })
      sock.once('error', () => resolve(false))
    })
  }
}
