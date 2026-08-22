import { describe, it, expect } from 'vitest'
// 验证 collectInstances 的去重逻辑（纯逻辑提取测试）
// 场景：remoteInstanceNames=["ljmacjxf"]，活跃客户端 jixiuf@ljmacjxf
// → ljmacjxf 应被 host 匹配覆盖，不产生重复条目

function dedup(active: Array<{ name: string; host?: string }>, remoteNames: string[]): Array<{ name: string; host?: string }> {
  const all: Array<{ name: string; host?: string }> = []
  for (const a of active) {
    if (!all.some((i) => i.name === a.name)) all.push(a)
  }
  for (const nm of remoteNames) {
    const covered =
      active.some((a) => a.name === nm) ||
      all.some((i) => i.host === nm) ||
      active.some((a) => a.host === nm)
    if (!covered) all.push({ name: nm })
  }
  return all
}

describe('实例列表去重', () => {
  it('remoteInstanceNames 的 hostname 被活跃客户端覆盖时不重复', () => {
    const active = [{ name: 'jixiuf', host: 'ljmacjxf' }]
    const result = dedup(active, ['ljmacjxf'])
    expect(result).toHaveLength(1)
    expect(result[0].name).toBe('jixiuf')
  })

  it('离线远程实例（无活跃客户端）保留', () => {
    const active: Array<{ name: string; host?: string }> = []
    const result = dedup(active, ['ljmacjxf'])
    expect(result).toHaveLength(1)
    expect(result[0].name).toBe('ljmacjxf')
  })
})
