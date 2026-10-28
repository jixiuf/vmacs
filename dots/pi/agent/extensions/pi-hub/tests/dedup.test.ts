import { describe, it, expect } from 'vitest'
import { sameInstance } from '../src/registry.js'
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

describe('跨机器同名实例去重（sameInstance）', () => {
  const local = { name: 'ddpushv2', pid: 1, cwd: '', sessionId: '', lastSeen: 0, host: 'ljmacjxf' }
  const remote = { name: 'ddpushv2', pid: 2, cwd: '', sessionId: '', lastSeen: 0, host: 'bj-vc-client-apm-01' }

  it('跨机器同名实例不被误判为同一实例', () => {
    expect(sameInstance(local, remote)).toBe(false)
  })

  it('同机同名实例判为同一实例（同 pid 重复注册场景）', () => {
    expect(sameInstance(local, { ...local })).toBe(true)
  })

  it('host 未知时退化为按 name 匹配（remoteInstanceNames 无 host 条目）', () => {
    expect(sameInstance(local, { name: 'ddpushv2' })).toBe(true)
    expect(sameInstance({ name: 'ddpushv2' }, remote)).toBe(true)
  })

  it('合并时远程同名实例不会被丢弃', () => {
    const all = [local]
    if (!all.some((i) => sameInstance(i, remote))) all.push(remote)
    expect(all).toHaveLength(2)
    expect(all.map((i) => i.host)).toEqual(['ljmacjxf', 'bj-vc-client-apm-01'])
  })
})
