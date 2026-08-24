// ============================================================================
// 统一日志：console（PI_COORDINATOR_DEBUG 开关）+ 事件落盘（自动大小轮转）
// 替代 index.ts 散落的 appendFileSync('/tmp/pi-coordinator-msg.log', ...)
// ============================================================================

import * as fs from 'node:fs'

const LOG_FILE = '/tmp/pi-hub.log'
/** 单文件大小上限：超过时轮转为 .old（保留最近一份），避免无限增长 */
const MAX_LOG_BYTES = 5 * 1024 * 1024

const isDebug = !!process.env.PI_COORDINATOR_DEBUG

/** 调试日志（仅 PI_COORDINATOR_DEBUG=1 时输出到 console） */
export function log(msg: string): void {
  if (isDebug) console.log(`[pi-hub] ${msg}`)
}

/** 事件日志（落盘 + 轮转）：扩展加载/会话生命周期/消息收发的可观测性 */
export function logEvent(kind: string, detail: string): void {
  try {
    const line = `[${new Date().toISOString()}] ${kind} ${detail}\n`
    // 大小轮转：超限时把当前文件滚动为 .old（保留最近一份），避免单文件无限增长
    try {
      const st = fs.statSync(LOG_FILE)
      if (st.size > MAX_LOG_BYTES) {
        try {
          fs.renameSync(LOG_FILE, `${LOG_FILE}.old`)
        } catch {
          fs.unlinkSync(LOG_FILE)
        }
      }
    } catch {
      // 文件不存在 → 直接追加
    }
    fs.appendFileSync(LOG_FILE, line)
  } catch {
    // 日志失败不影响主流程
  }
}
