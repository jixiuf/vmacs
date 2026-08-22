// ============================================================================
// 实例注册表：本机注册/注销/存活检测（instances.json）
// 由 coordinator.ts 拆出，职责单一：只回答"当前有哪些活着的实例"
// ============================================================================

import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import type { InstanceInfo } from './types.js'

const STATE_DIR =
  process.env.PI_HUB_STATE_DIR ?? path.join(os.homedir(), '.pi', 'agent', 'wechat-assistant')
const INSTANCES_FILE = path.join(STATE_DIR, 'instances.json')

function readJson<T>(file: string): T | null {
  try {
    return JSON.parse(fs.readFileSync(file, 'utf8')) as T
  } catch {
    return null
  }
}

function writeJson(file: string, data: unknown): void {
  try {
    fs.writeFileSync(file, JSON.stringify(data, null, 2), { mode: 0o600 })
  } catch {
    // ignore
  }
}

function isProcessRunning(pid: number): boolean {
  try {
    process.kill(pid, 0)
    return true
  } catch {
    return false
  }
}

export function registerInstance(info: Omit<InstanceInfo, 'lastSeen'>): void {
  const instances = readJson<Record<string, InstanceInfo>>(INSTANCES_FILE) ?? {}
  instances[info.name] = { ...info, host: info.host ?? os.hostname(), lastSeen: Date.now() }
  writeJson(INSTANCES_FILE, instances)
}

export function unregisterInstance(name: string, pid: number): void {
  const instances = readJson<Record<string, InstanceInfo>>(INSTANCES_FILE) ?? {}
  const entry = instances[name]
  if (entry && entry.pid === pid) {
    delete instances[name]
    writeJson(INSTANCES_FILE, instances)
  }
}

export function listInstances(): InstanceInfo[] {
  const instances = readJson<Record<string, InstanceInfo>>(INSTANCES_FILE) ?? {}
  const alive: InstanceInfo[] = []
  let changed = false
  for (const name of Object.keys(instances).sort()) {
    const info = instances[name]
    if (isProcessRunning(info.pid)) {
      alive.push(info)
    } else {
      delete instances[name]
      changed = true
    }
  }
  if (changed) writeJson(INSTANCES_FILE, instances)
  return alive
}
