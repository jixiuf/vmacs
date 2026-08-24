// ============================================================================
// 扩展 reload 生命周期清理注册表
//
// 背景：pi 扩展被 /reload 时，旧模块实例的闭包资源（WS 客户端、setInterval/setTimeout、
// 协调中心 server、HTTP 轮询定时器）不会自动销毁。旧 WS 闭包持有自己的重连定时器，
// 会继续重连并与新模块的连接在协调中心互相 destroy（服务器「新连接 destroy 旧同名连接」），
// 形成每秒重连风暴 / 假在线丢消息——这是历史上 WS 死循环与消息丢失的根源之一。
//
// 方案：所有长生命周期资源统一注册到这里；模块每次加载（含 reload）时先 disposeAllCleanups()
// 清理上一代全部资源，再注册新一代。旧 WS 闭包被 close()（closed=true）后，其 pending 的
// 重连 setTimeout 回调会在 connect() 入口被 closed 短路，彻底停止重连。
// ============================================================================

type CleanupFn = () => void

const registry = new Map<string, CleanupFn>()

/** 注册资源清理函数（同 key 覆盖旧注册） */
export function registerCleanup(key: string, fn: CleanupFn): void {
  registry.set(key, fn)
}

/** 注销资源清理函数（资源主动销毁时调用，避免 reload 时重复清理） */
export function unregisterCleanup(key: string): void {
  registry.delete(key)
}

/** 清理全部已注册资源并清空注册表（模块加载/reload 时调用，幂等） */
export function disposeAllCleanups(): void {
  for (const [key, fn] of [...registry]) {
    try {
      fn()
    } catch {
      // 单个资源清理失败不影响其余资源
    }
    registry.delete(key)
  }
}
