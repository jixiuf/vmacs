import { describe, it, expect, beforeEach, beforeAll, afterAll } from 'vitest'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { EnvelopeQueue } from '../src/queue.js'
import {
  coordinatorTryLock,
  coordinatorReleaseLock,
  getGlobalLockHolder,
  preassignLock,
} from '../src/lock.js'
import { toNumber, parseChineseNumber, normalizeCommand } from '../src/commands.js'

// 隔离状态目录，避免污染真实 ~/.pi 状态
const TEST_STATE_DIR = fs.mkdtempSync(path.join(os.tmpdir(), 'pi-hub-test-'))
process.env.PI_HUB_STATE_DIR = TEST_STATE_DIR

afterAll(() => {
  try {
    fs.rmSync(TEST_STATE_DIR, { recursive: true, force: true })
  } catch {
    // ignore
  }
})

describe('中文数字解析', () => {
  it('解析基础数字', () => {
    expect(parseChineseNumber('一')).toBe(1)
    expect(parseChineseNumber('十')).toBe(10)
    expect(parseChineseNumber('十二')).toBe(12)
    expect(parseChineseNumber('二十')).toBe(20)
    expect(parseChineseNumber('二十五')).toBe(25)
  })
  it('toNumber 混合输入', () => {
    expect(toNumber('2')).toBe(2)
    expect(toNumber('三')).toBe(3)
  })
})

describe('EnvelopeQueue 可靠消息队列', () => {
  let q: EnvelopeQueue
  beforeEach(() => {
    // 清理隔离目录中的持久化文件
    for (const f of ['coordinator-queue.json', 'coordinator-processed.json']) {
      try {
        fs.unlinkSync(path.join(TEST_STATE_DIR, f))
      } catch {
        // ignore
      }
    }
    q = new EnvelopeQueue()
  })

  it('按 to 取出，非目标消息保留', () => {
    q.enqueue({ type: 'message', id: 'm1', from: 'a', to: 'b', text: 'hello', ts: Date.now() })
    q.enqueue({ type: 'command', id: 'c1', from: 'a', to: 'b', command: '/reload', ts: Date.now() })
    q.enqueue({ type: 'message', id: 'm2', from: 'a', to: 'c', text: 'other', ts: Date.now() })

    const forB = q.dequeue('b')
    expect(forB).toHaveLength(2)
    expect(forB.map((x) => x.env.id)).toEqual(['m1', 'c1'])
    // m2 仍在队列，等 c 来取
    expect(q.dequeue('c')).toHaveLength(1)
  })

  it('ack 后不再取出；未 ack 保留', () => {
    q.enqueue({ type: 'message', id: 'm1', from: 'a', to: 'b', text: 'hello', ts: Date.now() })
    const first = q.dequeue('b')
    expect(first).toHaveLength(1)
    // 未 ack：再次 dequeue 不会重复给同一条（幂等去重），但消息仍在（等待重投）
    expect(q.dequeue('b')).toHaveLength(0)
    // ack 后彻底移除
    first[0].ack()
    expect(q.dequeue('b')).toHaveLength(0)
  })

  it('已处理 id 幂等去重（重投不重复进会话）', () => {
    q.enqueue({ type: 'message', id: 'dup-1', from: 'a', to: 'b', text: 'hello', ts: Date.now() })
    const items = q.dequeue('b')
    items[0].ack()
    // 模拟服务器重投同一条
    q.enqueue({ type: 'message', id: 'dup-1', from: 'a', to: 'b', text: 'hello', ts: Date.now() })
    expect(q.dequeue('b')).toHaveLength(0)
  })
})

describe('全局锁', () => {
  beforeEach(() => {
    coordinatorReleaseLock('test', 'wechat')
    coordinatorReleaseLock('other', 'wechat')
  })

  it('无锁时可获取；同 name+cap 续约', () => {
    expect(coordinatorTryLock('test', 1, 'wechat')).toBe(true)
    expect(coordinatorTryLock('test', 1, 'wechat')).toBe(true)
    expect(getGlobalLockHolder()?.name).toBe('test')
  })

  it('不同持有者不能抢占（除非 force）', () => {
    coordinatorTryLock('test', 1, 'wechat')
    expect(coordinatorTryLock('other', 2, 'wechat')).toBe(false)
    expect(coordinatorTryLock('other', 2, 'wechat', true)).toBe(true)
    expect(getGlobalLockHolder()?.name).toBe('other')
  })

  it('释放后 holder 为空', () => {
    coordinatorTryLock('test', 1, 'wechat')
    coordinatorReleaseLock('test', 'wechat')
    expect(getGlobalLockHolder()).toBeNull()
  })

  it('preassignLock 预占（接管让位）', () => {
    coordinatorTryLock('test', 1, 'wechat')
    preassignLock('new', 0, 'wechat')
    expect(getGlobalLockHolder()?.name).toBe('new')
  })
})

describe('命令归一化', () => {
  it('斜杠命令', () => {
    expect(normalizeCommand('/instances')).toEqual({ name: 'instances', rest: '' })
    expect(normalizeCommand('/msg home hello')).toEqual({ name: 'msg', rest: 'home hello' })
  })
  it('中文别名', () => {
    expect(normalizeCommand('实例列表')).toEqual({ name: 'instances', rest: '' })
    expect(normalizeCommand('所有实例')).toEqual({ name: 'instances', rest: '' })
  })
  it('非命令返回 null', () => {
    expect(normalizeCommand('你好')).toBeNull()
    expect(normalizeCommand('帮我看看代码')).toBeNull()
  })
})
