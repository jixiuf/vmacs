// ============================================================================
// 实例注册表：本机注册/注销/存活检测（instances.json）
// 由 coordinator.ts 拆出，职责单一：只回答"当前有哪些活着的实例"
// ============================================================================

import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import type { InstanceInfo } from './types.js'

// 惰性读取：测试环境可在 import 后设置 PI_HUB_STATE_DIR 隔离
function stateDir(): string {
  return process.env.PI_HUB_STATE_DIR ?? path.join(os.homedir(), '.pi', 'agent', 'wechat-assistant')
}
function instancesFile(): string {
  return path.join(stateDir(), 'instances.json')
}

function readJson<T>(file: string): T | null {
  try {
    return JSON.parse(fs.readFileSync(file, 'utf8')) as T
  } catch {
    return null
  }
}

function writeJson(file: string, data: unknown): void {
  try {
    // 原子写：tmp + rename，避免并发/崩溃造成半写文件（半写会令下次 readJson 失败丢全部条目）
    const tmp = `${file}.tmp`
    fs.writeFileSync(tmp, JSON.stringify(data, null, 2), { mode: 0o600 })
    fs.renameSync(tmp, file)
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

/**
 * 从实例名剥离末尾已附加的 pid 及可选随机后缀，得到基础名。
 * 如 `models-1313-1234` / `models-1313-abcd` → `models`。
 * 防止多次改名后实例名无限变长（models-1313-1234-5678...）。
 */
export function baseInstanceName(name: string): string {
  return name.replace(/-\d{2,7}(-[0-9a-z]{4})?$/, '')
}

export function registerInstance(info: Omit<InstanceInfo, 'lastSeen'>): string {
  const instances = readJson<Record<string, InstanceInfo>>(instancesFile()) ?? {}
  // 同 pid 去重：同一进程（pid）重复注册时只保留一条。
  // 扩展 reloadall / IM 桥接多会话会令同一进程多次触发注册（fallback 或 session_start），
  // 旧条目不清理则同 pid 无限累积，且 pruneInstances 按 pid 存活判断永远清不掉。
  const samePidKeys = Object.keys(instances).filter((key) => instances[key].pid === info.pid)
  if (samePidKeys.length > 0) {
    for (const key of samePidKeys) delete instances[key]
  }
  // 名字从基础名重建（剥离已附加的 pid/随机后缀），避免派生/reload 后越加越长
  let name = baseInstanceName(info.name)
  // 同名冲突：已有同名且存活（非自身）→ 加 pid 后缀，保证实例名唯一，避免覆盖
  const existing = instances[name]
  if (existing && existing.pid !== info.pid && isProcessRunning(existing.pid)) {
    name = `${name}-${info.pid}`
    // 极端情况仍冲突（多个同名存活）→ 追加随机后缀
    while (instances[name] && isProcessRunning(instances[name].pid)) {
      name = `${baseInstanceName(info.name)}-${info.pid}-${Math.random().toString(36).slice(2, 6)}`
    }
  }
  instances[name] = { ...info, name, host: info.host ?? os.hostname(), lastSeen: Date.now() }
  writeJson(instancesFile(), instances)
  return name
}

export function unregisterInstance(name: string, pid: number): void {
  const instances = readJson<Record<string, InstanceInfo>>(instancesFile()) ?? {}
  const entry = instances[name]
  if (entry && entry.pid === pid) {
    delete instances[name]
    writeJson(instancesFile(), instances)
  }
}

export function listInstances(): InstanceInfo[] {
  // 只读，不写回：多实例并发读-改-写会把注册表清空（本机曾因此丢全部条目导致 envelope 投递中断）。
  // 清理由注册/注销 + 定时 pruneInstances 完成，这里只回答“当前活着的本地实例”。
  const instances = readJson<Record<string, InstanceInfo>>(instancesFile()) ?? {}
  const localHost = os.hostname()
  const alive: InstanceInfo[] = []
  for (const name of Object.keys(instances).sort()) {
    const info = instances[name]
    // 只认本机条目（host 为空视为本机）；远程条目不参与本地存活判断（pid 无法在本机检查）
    if (info.host && info.host !== localHost) continue
    if (isProcessRunning(info.pid)) alive.push(info)
  }
  return alive
}

/** 清理已死亡的本机条目（仅本机，原子写）。由独立定时器低频调用，避免与注册/注销并发竞争。 */
export function pruneInstances(): void {
  const instances = readJson<Record<string, InstanceInfo>>(instancesFile()) ?? {}
  const localHost = os.hostname()
  let changed = false
  for (const name of Object.keys(instances)) {
    const info = instances[name]
    if (info.host && info.host !== localHost) continue
    if (!isProcessRunning(info.pid)) {
      delete instances[name]
      changed = true
    }
  }
  if (changed) writeJson(instancesFile(), instances)
}
