import { describe, it, expect } from 'vitest'
import { spawnSync } from 'node:child_process'
import * as fs from 'node:fs'
import * as path from 'node:path'
import { coordinatorReleaseLock } from '../src/lock.js'

// TOCTOU 竞态测试：两个独立进程同时 coordinatorTryLock（各自真实 pid），只有一个应成功
describe('锁获取 TOCTOU 竞态', () => {
  it('并发获取：只有一个进程成功持锁', () => {
    const stateDir = process.env.PI_HUB_STATE_DIR
    expect(stateDir).toBeTruthy()
    try { fs.unlinkSync(path.join(stateDir!, 'coordinator-lock.json')) } catch { /* ignore */ }

    // 两个进程并发抢锁（子进程用自己的真实 pid）
    const script = `
      import { coordinatorTryLock } from 'file://${process.cwd()}/src/lock.ts'
      const ok = coordinatorTryLock('home', process.pid, 'wechat')
      console.log(ok ? 'LOCKED' : 'DENIED')
    `
    const results: string[] = []
    for (let i = 0; i < 2; i++) {
      const r = spawnSync('node', ['--experimental-strip-types', '--input-type=module', '-e', script], {
        encoding: 'utf8',
        timeout: 10000,
        env: { ...process.env, PI_HUB_STATE_DIR: stateDir },
      })
      results.push(r.stdout.trim())
    }
    coordinatorReleaseLock('home', 'wechat')
    try { fs.unlinkSync(path.join(stateDir!, 'coordinator-lock.json')) } catch { /* ignore */ }

    const locked = results.filter((r) => r === 'LOCKED').length
    expect(locked).toBe(1) // 串行执行下第一个成功第二个拒绝
  }, 20_000)
})
