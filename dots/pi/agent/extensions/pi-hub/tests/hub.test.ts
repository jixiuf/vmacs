import { describe, it, expect, beforeEach, beforeAll, afterAll } from 'vitest'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { EnvelopeQueue } from '../src/queue.js'
import { registerInstance, listInstances } from '../src/registry.js'
import {
  coordinatorTryLock,
  coordinatorReleaseLock,
  getGlobalLockHolder,
  preassignLock,
} from '../src/lock.js'
import { toNumber, parseChineseNumber, normalizeCommand, tryLooseUse, executeCommand, type CommandCtx } from '../src/commands.js'

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
    coordinatorReleaseLock('new', 'wechat')
    coordinatorReleaseLock('instance-a', 'wechat')
    coordinatorReleaseLock('instance-b', 'wechat')
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

  it('同名不同 pid 不能同时持锁（多实例互斥核心）', () => {
    // 模拟两个实例名相同（如 config.instanceName 都是 home）但进程不同
    expect(coordinatorTryLock('home', 1001, 'wechat')).toBe(true)
    // 同 pid 续约 → 成功
    expect(coordinatorTryLock('home', 1001, 'wechat')).toBe(true)
    // 不同 pid（另一进程，同名同 capability）→ 必须失败（不能双轮询）
    expect(coordinatorTryLock('home', 2002, 'wechat')).toBe(false)
    // 释放需带 pid（1001 释放；2002 释放无效）
    coordinatorReleaseLock('home', 'wechat', 2002)
    expect(getGlobalLockHolder()?.name).toBe('home')
    coordinatorReleaseLock('home', 'wechat', 1001)
    expect(getGlobalLockHolder()).toBeNull()
  })

  it('预占锁 pid=0 可被同名实例接管', () => {
    preassignLock('home', 0, 'wechat')
    expect(getGlobalLockHolder()?.name).toBe('home')
    // 目标实例用真实 pid 接管预占锁
    expect(coordinatorTryLock('home', 3003, 'wechat')).toBe(true)
    expect(getGlobalLockHolder()).toEqual({ name: 'home', capability: 'wechat' })
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

describe('宽松 use 匹配（说「2」/「use home」/「home」直接切换并消费）', () => {
  const instances = (names: string[]) =>
    names.map((name, i) => ({
      name,
      pid: i + 1,
      cwd: '',
      sessionId: '',
      lastSeen: Date.now(),
      host: undefined,
    }))

  it('纯数字命中有效编号', () => {
    expect(tryLooseUse('2', instances(['extensions', 'home']))).toEqual({ name: 'use', rest: '2' })
  })
  it('单实例时不匹配纯数字（避免误吞普通数字）', () => {
    expect(tryLooseUse('2', instances(['extensions']))).toBeNull()
    expect(tryLooseUse('1', instances(['extensions']))).toBeNull()
  })
  it('编号超出实例范围不匹配', () => {
    expect(tryLooseUse('3', instances(['extensions', 'home']))).toBeNull()
  })
  it('中文数字也识别', () => {
    expect(tryLooseUse('二', instances(['extensions', 'home']))).toEqual({ name: 'use', rest: '2' })
  })
  it('无斜杠 use 形式', () => {
    expect(tryLooseUse('use home', instances(['extensions', 'home']))).toEqual({ name: 'use', rest: 'home' })
    expect(tryLooseUse('USE 2', instances(['extensions', 'home']))).toEqual({ name: 'use', rest: '2' })
  })
  it('整句等于实例名（忽略大小写）', () => {
    expect(tryLooseUse('home', instances(['extensions', 'home']))).toEqual({ name: 'use', rest: 'home' })
    expect(tryLooseUse('Home', instances(['extensions', 'home']))).toEqual({ name: 'use', rest: 'home' })
  })
  it('普通对话不匹配', () => {
    const all = instances(['extensions', 'home'])
    expect(tryLooseUse('你好', all)).toBeNull()
    expect(tryLooseUse('帮我看看代码', all)).toBeNull()
    expect(tryLooseUse('给我两个方案', all)).toBeNull()
    expect(tryLooseUse('/instances', all)).toBeNull()
    expect(tryLooseUse('今天2点开会', all)).toBeNull()
  })
})

describe('executeCommand 宽松 use 执行', () => {
  const makeCtx = (): CommandCtx => ({
    currentInstanceName: 'extensions',
    collectInstances: async () => ({
      local: [],
      all: [
        { name: 'extensions', pid: 1, cwd: '', sessionId: '', lastSeen: Date.now(), host: undefined },
        { name: 'home', pid: 2, cwd: '', sessionId: '', lastSeen: Date.now(), host: undefined },
      ],
    }),
    resolveTarget: () => undefined,
    doSwitch: async (name) => `switched ${name}`,
    doSendCommand: async () => '',
    doSendMessage: async () => '',
    rememberTarget: () => {},
    getLastTarget: () => null,
    doStartPi: async () => '',
    doReloadAll: async () => 'reloaded all',
    writeClipboard: async () => true,
  })

  it('说「2」执行 use 并消费（不投递给 agent）', async () => {
    expect(await executeCommand('2', makeCtx())).toEqual({ reply: 'switched 2', consumed: true })
  })
  it('「use home」执行切换', async () => {
    expect(await executeCommand('use home', makeCtx())).toEqual({ reply: 'switched home', consumed: true })
  })
  it('loose=false（问卷等待中）不消费普通数字', async () => {
    expect(await executeCommand('2', makeCtx(), { loose: false })).toBeNull()
  })
  it('普通对话返回 null', async () => {
    expect(await executeCommand('你好', makeCtx())).toBeNull()
  })
})

describe('start 命令（在实例 tmux 中启动 pi）', () => {
  const makeCtx = (): CommandCtx => ({
    currentInstanceName: 'extensions',
    collectInstances: async () => ({
      local: [],
      all: [
        { name: 'extensions', pid: 1, cwd: '/home/ext', sessionId: '', lastSeen: Date.now(), host: 'ljmacjxf' },
        { name: 'home', pid: 2, cwd: '/home/admin', sessionId: '', lastSeen: Date.now(), host: 'bj-vc-client-apm-01' },
      ],
    }),
    resolveTarget: (all, name) => all.find((i) => i.name === name),
    doSwitch: async (name) => `switched ${name}`,
    doSendCommand: async () => '',
    doSendMessage: async () => '',
    rememberTarget: () => {},
    getLastTarget: () => null,
    doStartPi: async (target, cwd) => `started ${target.name}${cwd ? ` @${cwd}` : ''}`,
    doReloadAll: async () => 'reloaded all',
    writeClipboard: async () => true,
  })

  it('斜杠 /start home 命中并转发目录', async () => {
    expect(await executeCommand('/start home', makeCtx())).toEqual({ reply: 'started home', consumed: true })
  })
  it('英文 start pi 命中', async () => {
    expect(await executeCommand('start pi home', makeCtx())).toEqual({ reply: 'started home', consumed: true })
  })
  it('中文别名「启动pi」命中', async () => {
    expect(await executeCommand('启动pi home', makeCtx())).toEqual({ reply: 'started home', consumed: true })
  })
  it('带目录参数', async () => {
    expect(await executeCommand('/start home ~/proj', makeCtx())).toEqual({ reply: 'started home @~/proj', consumed: true })
  })
  it('无参数时默认当前实例（本机）', async () => {
    expect(await executeCommand('/start', makeCtx())).toEqual({ reply: 'started extensions', consumed: true })
  })
  it('第一个参数不是实例名时作为目录，默认本机', async () => {
    expect(await executeCommand('/start ~/proj', makeCtx())).toEqual({ reply: 'started extensions @~/proj', consumed: true })
  })
  it('打错的实例名（非目录）报错不静默当目录', async () => {
    expect(await executeCommand('/start home2', makeCtx())).toEqual({ reply: '未找到实例 home2，先 /instances 查看', consumed: true })
  })
  it('中文别名无实例默认本机', async () => {
    expect(await executeCommand('启动pi', makeCtx())).toEqual({ reply: 'started extensions', consumed: true })
  })
  it('普通对话不命中 start', async () => {
    expect(await executeCommand('帮我在服务器上启动pi', makeCtx())).toBeNull()
  })
})

describe('reloadall 命令', () => {
  const makeCtx = (): CommandCtx => ({
    currentInstanceName: 'extensions',
    collectInstances: async () => ({
      local: [],
      all: [
        { name: 'extensions', pid: 1, cwd: '', sessionId: '', lastSeen: Date.now(), host: undefined },
        { name: 'home', pid: 2, cwd: '', sessionId: '', lastSeen: Date.now(), host: undefined },
      ],
    }),
    resolveTarget: () => undefined,
    doSwitch: async () => '',
    doSendCommand: async () => '',
    doSendMessage: async () => '',
    rememberTarget: () => {},
    getLastTarget: () => null,
    doStartPi: async () => '',
    doReloadAll: async () => 'reloaded extensions, home',
  })

  it('斜杠 /reloadall 命中并调用 doReloadAll', async () => {
    expect(await executeCommand('/reloadall', makeCtx())).toEqual({ reply: 'reloaded extensions, home', consumed: true })
  })
  it('中文别名「重载全部」命中', async () => {
    expect(await executeCommand('重载全部', makeCtx())).toEqual({ reply: 'reloaded extensions, home', consumed: true })
  })
  it('英文「reload all」命中', async () => {
    expect(await executeCommand('reload all', makeCtx())).toEqual({ reply: 'reloaded extensions, home', consumed: true })
  })
  it('普通对话不命中', async () => {
    expect(await executeCommand('重载一下这个', makeCtx())).toBeNull()
  })
})

describe('实例注册唯一性（同名存活冲突自动改名）', () => {
  it('同名但旧 pid 已死 → 正常覆盖（回收名字）', () => {
    // 假 pid（进程不存在）→ isProcessRunning 视为已死 → 正常覆盖
    const n1 = registerInstance({ name: 'same', pid: 999991, cwd: '/tmp', sessionId: 's1', host: 'h' })
    const n2 = registerInstance({ name: 'same', pid: 999992, cwd: '/tmp', sessionId: 's2', host: 'h' })
    expect(n1).toBe('same')
    expect(n2).toBe('same') // 旧 pid 已死 → 回收名字，不触发改名
  })

  it('同名且旧 pid 存活 → 新实例自动改名（-pid），不覆盖', () => {
    // 用当前进程真实 pid 模拟存活冲突
    const alivePid = process.pid
    const n1 = registerInstance({ name: 'dup', pid: alivePid, cwd: '/tmp', sessionId: 'd1', host: 'h' })
    const n2 = registerInstance({ name: 'dup', pid: alivePid + 1, cwd: '/tmp', sessionId: 'd2', host: 'h' })
    expect(n1).toBe('dup')
    expect(n2).toBe(`dup-${alivePid + 1}`) // 存活冲突 → 新实例改名，两个并存
  })
})
