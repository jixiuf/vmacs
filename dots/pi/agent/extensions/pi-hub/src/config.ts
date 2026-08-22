// ============================================================================
// pi-hub 配置：单一 schema，替代 coordinator/config.json 与 wechat-assistant/config.json 双份
// 向后兼容：优先读自身 config，缺失字段回退 wechat-assistant config（通道互通）
// ============================================================================

import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'

export interface HubConfig {
  instanceName?: string
  coordinatorPort?: number
  coordinatorUrl?: string
  remoteInstanceNames?: string[]
  remoteHosts?: Record<string, { target: string; port?: number }>
  /** 渠道开关：channels.wechat.enabled 等（未来 feishu 同构） */
  channels?: Record<string, { enabled?: boolean; autoTakeover?: boolean }>
}

const HUB_CONFIG_DIR = path.join(os.homedir(), '.pi', 'agent', 'coordinator')
const HUB_CONFIG_FILE = path.join(HUB_CONFIG_DIR, 'config.json')
const WECHAT_CONFIG_FILE = path.join(os.homedir(), '.pi', 'agent', 'wechat-assistant', 'config.json')

function readJson<T>(file: string): T | null {
  try {
    return JSON.parse(fs.readFileSync(file, 'utf8')) as T
  } catch {
    return null
  }
}

/** 读取协调配置：优先自己的 config，缺失字段回退 wechat-assistant 配置（通道互通） */
export function loadHubConfig(): HubConfig {
  const own = readJson<HubConfig>(HUB_CONFIG_FILE) ?? {}
  const wechat = readJson<HubConfig>(WECHAT_CONFIG_FILE) ?? {}
  const cfg: HubConfig = {
    instanceName: own.instanceName ?? wechat.instanceName,
    coordinatorPort: own.coordinatorPort ?? wechat.coordinatorPort,
    coordinatorUrl: own.coordinatorUrl ?? wechat.coordinatorUrl,
    remoteInstanceNames: own.remoteInstanceNames ?? wechat.remoteInstanceNames,
    remoteHosts: own.remoteHosts ?? wechat.remoteHosts,
    channels: own.channels ?? wechat.channels,
  }
  // 配置文件缺失/损坏时自动重建，避免配置真空（实例名丢失导致消息拉取失效）
  if (!readJson<unknown>(HUB_CONFIG_FILE)) {
    try {
      fs.mkdirSync(HUB_CONFIG_DIR, { recursive: true })
      fs.writeFileSync(HUB_CONFIG_FILE, JSON.stringify(cfg, null, 2))
    } catch {
      // ignore
    }
  }
  return cfg
}

/** 渠道是否启用（默认启用，除非显式 disabled） */
export function isChannelEnabled(cfg: HubConfig, kind: string): boolean {
  return cfg.channels?.[kind]?.enabled ?? true
}

/** 渠道是否自动接管（默认 true） */
export function isChannelAutoTakeover(cfg: HubConfig, kind: string): boolean {
  return cfg.channels?.[kind]?.autoTakeover ?? true
}
