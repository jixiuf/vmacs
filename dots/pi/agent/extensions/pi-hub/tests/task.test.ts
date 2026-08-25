import { describe, it, expect, beforeEach } from 'vitest'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { TaskRegistry, TASK_TTL_MS, TASK_MAX_ATTEMPTS, extractTaskId, extractReplyText, hasManualTaskReply } from '../src/task.js'

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
    const t = reg.create({ assignee, payload: `task-${i}`, isSubagent: true })
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
    const t = reg.create({ assignee: 'sub1', payload: 'p', keep: true, isSubagent: true })
    reg.update(t.id, { status: 'done' })
    const events = reg.monitor()
    expect(events.some((e) => e.kind === 'reclaim' && e.assignee === 'sub1')).toBe(false)
  })

  it('回归：无 isSubagent 标记的 done 任务不触发回收（assignee 撞实例名也不误杀）', () => {
    const reg = new TaskRegistry()
    // 模拟历史遗留任务：dispatch_task 分发给 "admin"（无 isSubagent 标记），已完成
    const t = reg.create({ assignee: 'admin', payload: '旧任务' })
    reg.update(t.id, { status: 'done', result: 'ok' })
    const events = reg.monitor()
    expect(events.some((e) => e.kind === 'reclaim' && e.assignee === 'admin')).toBe(false)
  })

  it('回归：dispatch_task 分发的新任务（无 isSubagent）完成后也不回收', () => {
    const reg = new TaskRegistry()
    const t = reg.create({ assignee: 'src-12345', payload: '协作任务' })
    reg.update(t.id, { status: 'done' })
    const events = reg.monitor()
    expect(events.some((e) => e.kind === 'reclaim' && e.assignee === 'src-12345')).toBe(false)
  })

  it('本系统 subagent（isSubagent）任务完成 → reclaim；同一 assignee 的旧任务不污染判定', () => {
    const reg = new TaskRegistry()
    // 历史遗留任务（无标记，done）+ 本系统 subagent 任务（isSubagent，done）
    const old = reg.create({ assignee: 'sub1', payload: 'old' })
    reg.update(old.id, { status: 'done' })
    const sub = reg.create({ assignee: 'sub1', payload: 'new', isSubagent: true })
    reg.update(sub.id, { status: 'done' })
    const events = reg.monitor()
    expect(events).toContainEqual(expect.objectContaining({ kind: 'reclaim', assignee: 'sub1' }))
  })

  it('instanceDone 对无任务实例返回 false', () => {
    const reg = new TaskRegistry()
    expect(reg.instanceDone('ghost')).toBe(false)
  })
})

describe('任务协议纯函数', () => {
  it('extractTaskId 提取 TASK-id', () => {
    expect(extractTaskId('请处理，任务ID: TASK-1787553502281-kgd9')).toBe('TASK-1787553502281-kgd9')
    expect(extractTaskId('[TASK-123-abc结果] {"status":"done"}')).toBe('TASK-123-abc')
    expect(extractTaskId('普通消息没有任务')).toBeNull()
  })

  it('extractReplyText 提取字符串 content', () => {
    const branch = [{ type: 'message', message: { role: 'user', content: 'hi' } }, { type: 'message', message: { role: 'assistant', content: '你好，结果是 X' } }]
    expect(extractReplyText(branch)).toBe('你好，结果是 X')
  })

  it('extractReplyText 提取数组 content（text 块拼接）', () => {
    const branch = [{ type: 'message', message: { role: 'assistant', content: [{ type: 'text', text: '{"status":"done"}' }, { type: 'text', text: '完成' }] } }]
    expect(extractReplyText(branch)).toContain('"status":"done"')
    expect(extractReplyText(branch)).toContain('完成')
  })

  it('extractReplyText 无 assistant 消息返回空串', () => {
    expect(extractReplyText([])).toBe('')
    expect(extractReplyText([{ type: 'message', message: { role: 'user', content: 'x' } }])).toBe('')
  })

  it('extractReplyText 截断超长内容', () => {
    const long = 'x'.repeat(5000)
    const branch = [{ type: 'message', message: { role: 'assistant', content: long } }]
    expect(extractReplyText(branch, 100).length).toBe(100)
  })
})

describe('hasManualTaskReply（手动回传跳过）', () => {
  it('回复含 [TASK-id结果] 前缀 → true（已手动回传，跳过自动回传）', () => {
    expect(hasManualTaskReply('[TASK-123-abc结果] {"status":"done"}', 'TASK-123-abc')).toBe(true)
    expect(hasManualTaskReply('已完成。\n[TASK-123-abc结果] 结果如下', 'TASK-123-abc')).toBe(true)
  })

  it('回复不含该任务 ID → false（需要自动回传）', () => {
    expect(hasManualTaskReply('我完成了任务，结果如下', 'TASK-123-abc')).toBe(false)
    expect(hasManualTaskReply('[TASK-999-xyz结果] 别的任务', 'TASK-123-abc')).toBe(false)
  })

  it('ID 前缀不完整（无 ]）不匹配', () => {
    expect(hasManualTaskReply('[TASK-123-abc结果', 'TASK-123-abc')).toBe(false)
  })
})
