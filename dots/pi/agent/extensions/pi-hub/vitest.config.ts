import { defineConfig } from 'vitest/config'
import * as fs from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'

// 隔离状态目录（在模块加载前注入环境变量）
const testStateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'pi-hub-test-'))

export default defineConfig({
  test: {
    include: ['tests/**/*.test.ts'],
    environment: 'node',
    env: {
      PI_HUB_STATE_DIR: testStateDir,
    },
  },
})
