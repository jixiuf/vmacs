// 每个测试文件运行前清理锁文件，避免跨文件状态竞态
import { beforeEach } from 'vitest'
import * as fs from 'node:fs'
import * as path from 'node:path'

beforeEach(() => {
  const dir = process.env.PI_HUB_STATE_DIR
  if (!dir) return
  try {
    fs.unlinkSync(path.join(dir, 'coordinator-lock.json'))
  } catch {
    // ignore
  }
})
