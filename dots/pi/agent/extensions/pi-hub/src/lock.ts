// ============================================================================
// 全局锁 + 接管仲裁：coordinator-lock.json（跨机器：谁在提供服务/轮询）
// 由 coordinator.ts 拆出，同时吸收 wechat/instances.ts 中重复的锁逻辑
// ============================================================================

import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import type { GlobalLock } from './types.js'

const STATE_DIR =
  process.env.PI_HUB_STATE_DIR ?? path.join(os.homedir(), '.pi', 'agent', 'wechat-assistant')
const GLOBAL_LOCK_FILE = path.join(STATE_DIR, 'coordinator-lock.json')

/** 全局锁心跳超时：超过此时长未续约视为持有者下线 */
export const GLOBAL_LOCK_TTL_MS = 10_000

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

/** 原子创建锁文件：仅当文件不存在时成功（防止多进程同时读空后都写入的 TOCTOU 竞态） */
function createLockFileExclusive(data: GlobalLock): boolean {
  try {
    const fd = fs.openSync(GLOBAL_LOCK_FILE, 'wx', 0o600)
    try {
      fs.writeFileSync(fd, JSON.stringify(data, null, 2))
    } finally {
      fs.closeSync(fd)
    }
    return true
  } catch {
    return false // 文件已存在（他人持有或竞态失败）
  }
}

function readGlobalLockFile(): GlobalLock | null {
  const d = readJson<GlobalLock>(GLOBAL_LOCK_FILE)
  return d && d.name ? d : null
}

/**
 * 尝试获取/续约全局锁。
 * - 无持有者 → 直接获取
 * - 持有者是自己（同 name + 同 pid + 同 capability）→ 续约
 * - 持有者 pid=0（接管预占，目标未定）→ 同 name 可接管（写真实 pid）
 * - 持有者是别人：force 或已超时 → 抢占；否则失败
 */
export function coordinatorTryLock(name: string, pid: number, capability?: string, force = false): boolean {
  const now = Date.now()
  try {
    if (process.env.PI_HUB_LOCK_DEBUG) {
      const st = new Error().stack?.split('\n')[2]?.trim() ?? '?'
      fs.appendFileSync(path.join(STATE_DIR, 'lock-debug.log'), `[${new Date().toISOString()}] tryLock name=${name} pid=${pid} cap=${capability} force=${force} from=${st}\n`)
    }
  } catch { /* ignore */ }
  const cur = readGlobalLockFile()
  if (cur) {
    const sameName = cur.name === name
    const sameCap = cur.capability === capability
    // 自己：name + pid + capability 全部一致 → 续约
    if (sameName && sameCap && cur.pid === pid) {
      writeJson(GLOBAL_LOCK_FILE, { name, pid, capability, lastSeen: now })
      return true
    }
    // 接管预占：pid=0 表示目标未定，同 name 的实例可接管（写入真实 pid）
    if (sameName && sameCap && cur.pid === 0) {
      writeJson(GLOBAL_LOCK_FILE, { name, pid, capability, lastSeen: now })
      return true
    }
    // 别人持有：force 或 TTL 超时才可抢占
    if (force || now - cur.lastSeen > GLOBAL_LOCK_TTL_MS) {
      writeJson(GLOBAL_LOCK_FILE, { name, pid, capability, lastSeen: now })
      return true
    }
    return false
  }
  // 无持有者：原子创建（wx 独占），防止两个进程同时读空后都写成功
  if (createLockFileExclusive({ name, pid, capability, lastSeen: now })) return true
  // 创建失败：他人刚写入，重读判断
  const after = readGlobalLockFile()
  if (!after) return false
  if (after.name === name && after.capability === capability && (after.pid === pid || after.pid === 0)) {
    writeJson(GLOBAL_LOCK_FILE, { name, pid, capability, lastSeen: Date.now() })
    return true
  }
  return false
}

export function coordinatorReleaseLock(name: string, capability?: string, pid?: number): void {
  const cur = readGlobalLockFile()
  if (
    cur &&
    cur.name === name &&
    (!capability || cur.capability === capability) &&
    (pid === undefined || cur.pid === pid)
  ) {
    // 删除文件（而非写空对象），让后续 wx 原子创建能成功
    try {
      fs.unlinkSync(GLOBAL_LOCK_FILE)
    } catch {
      // ignore
    }
  }
}

export function getGlobalLockHolder(): { name: string; capability?: string } | null {
  const cur = readGlobalLockFile()
  if (!cur) return null
  if (Date.now() - cur.lastSeen > GLOBAL_LOCK_TTL_MS) {
    try {
      fs.unlinkSync(GLOBAL_LOCK_FILE)
    } catch {
      // ignore
    }
    return null
  }
  return { name: cur.name, capability: cur.capability }
}

/** 预占全局锁：让当前持有者心跳失败自动让位，目标实例可续约接管 */
export function preassignLock(name: string, pid: number, capability?: string): void {
  if (!capability) return
  writeJson(GLOBAL_LOCK_FILE, {
    name,
    pid,
    capability,
    lastSeen: Date.now(),
  })
}
