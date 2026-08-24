import { describe, it, expect, beforeEach } from 'vitest'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { TaskRegistry, TASK_TTL_MS, TASK_MAX_ATTEMPTS } from '../src/task.js'

// 隔离状态目录：vitest 每文件独立进程，这里用临时目录避免污染真实 tasks.json
const testDir = path.join(os.tmpdir(), `pi-hub-task-test-${process.pid}`)
process.env.PI_HUB_STATE_DIR = testDir

beforeEach(() => {
  try {
    fs.unlinkSync(path.join(testDir, 'tasks.json'))
  } catch {
    // ignore
  }
})

function createDoneTasks(reg: TaskRegistry, assignee: string, n: number): void {
  for (let i = 0; i < n; i++) {
    const t = reg.create({ assignee, payload: `task-${i}` })
    reg.update(t.id, { status: 'done', result: 'ok' })
  }
}

describe('TaskRegistry CRUD', () => {
  it('create 返回带状态/截止时间的任务并持久化', () => {
    const reg = new TaskRegistry()
    const t = reg.create({ assignee: 'sub1', payload: '分析代码' })
    expect(t.id.startsWith('TASK-')).toBe(true)
    expect(t.status).toBe('pending')
    expect(t.deadline).toBeGreaterThan(t.createdAt)
    // 持久化：新实例能读回
    const reg2 = new TaskRegistry()
    expect(reg2.get(t.id)?.assignee).toBe('sub1')
  })

  it('update 修改状态/结果', () => {
    const reg = new TaskRegistry()
    const t = reg.create({ assignee: 'sub1', payload: 'p' })
    reg.update(t.id, { status: 'done', result: '{"ok":true}' })
    expect(reg.get(t.id)?.status).toBe('done')
    expect(reg.get(t.id)?.result).toBe('{"ok":true}')
  })

  it('list 返回全部任务', () => {
    const reg = new TaskRegistry()
    reg.create({ assignee: 'a', payload: '1' })
    reg.create({ assignee: 'b', payload: '2' })
    expect(reg.list().length).toBe(2)
  })
})

describe('TaskRegistry.monitor 超时', () => {
  it('超时未达上限 → retry（attempts+1，回到 pending，重置 deadline）', () => {
    const reg = new TaskRegistry()
    const t = reg.create({ assignee: 'sub1', payload: 'p', ttlMs: 1000 })
    const now = t.createdAt + 2000 // 已超时
    const events = reg.monitor(now)
    expect(events).toContainEqual(expect.objectContaining({ id: t.id, kind: 'retry' }))
    const after = reg.get(t.id)!
    expect(after.status).toBe('pending')
    expect(after.attempts).toBe(1)
    expect(after.deadline).toBeGreaterThan(now)
  })

  it('超时达上限 → timeout', () => {
    const reg = new TaskRegistry()
    const t = reg.create({ assignee: 'sub1', payload: 'p', ttlMs: 1000 })
    // 直接模拟已达上限
    reg.update(t.id, { attempts: TASK_MAX_ATTEMPTS })
    const events = reg.monitor(t.createdAt + 2000)
    expect(events).toContainEqual(expect.objectContaining({ id: t.id, kind: 'timeout' }))
    expect(reg.get(t.id)?.status).toBe('timeout')
  })

  it('未超时任务不产生事件', () => {
    const reg = new TaskRegistry()
    const t = reg.create({ assignee: 'sub1', payload: 'p', ttlMs: 60_000 })
    expect(reg.monitor(t.createdAt + 1000).length).toBe(0)
  })
})

describe('TaskRegistry 回收判定', () => {
  it('实例全部任务结束 → reclaim', () => {
    const reg = new TaskRegistry()
    createDoneTasks(reg, 'sub1', 2)
    const events = reg.monitor()
    expect(events).toContainEqual(expect.objectContaining({ kind: 'reclaim', assignee: 'sub1' }))
  })

  it('有未完成任务 → 不 reclaim', () => {
    const reg = new TaskRegistry()
    createDoneTasks(reg, 'sub1', 1)
    reg.create({ assignee: 'sub1', payload: 'still-running' }) // pending
    const events = reg.monitor()
    expect(events.some((e) => e.kind === 'reclaim' && e.assignee === 'sub1')).toBe(false)
  })

  it('keep 任务不触发回收', () => {
    const reg = new TaskRegistry()
    const t = reg.create({ assignee: 'sub1', payload: 'p', keep: true })
    reg.update(t.id, { status: 'done' })
    const events = reg.monitor()
    expect(events.some((e) => e.kind === 'reclaim' && e.assignee === 'sub1')).toBe(false)
  })

  it('instanceDone 对无任务实例返回 false', () => {
    const reg = new TaskRegistry()
    expect(reg.instanceDone('ghost')).toBe(false)
  })
})
