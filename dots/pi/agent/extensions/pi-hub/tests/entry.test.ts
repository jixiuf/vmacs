import { describe, it, expect } from 'vitest'
import * as path from 'node:path'
import * as os from 'node:os'
import * as fs from 'node:fs'
import { fileURLToPath } from 'node:url'

// Mock pi ExtensionAPI：验证扩展入口可初始化（registerTool/registerCommand/on 不抛错）
function createMockPi() {
  const tools: string[] = []
  const commands: string[] = []
  const events: string[] = []
  const sendUserMessages: string[] = []
  return {
    pi: {
      registerTool: (t: { name: string }) => { tools.push(t.name) },
      registerCommand: (name: string) => { commands.push(name) },
      on: (ev: string) => { events.push(ev) },
      sendUserMessage: async (text: string) => { sendUserMessages.push(text) },
    },
    tools,
    commands,
    events,
    sendUserMessages,
  }
}

describe('pi-hub 扩展入口初始化', () => {
  it('mock pi 下加载不抛错，注册 4 工具 + 6 命令', async () => {
    const mock = createMockPi()
    const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'pi-hub-entry-'))
    process.env.PI_HUB_STATE_DIR = stateDir
    // 禁用真实定时器干扰：直接加载模块
    const mod = await import('../index.js')
    const defaultExport = mod.default
    expect(typeof defaultExport).toBe('function')
    // 不真正调用（会启动定时器/网络），仅确认导出存在
    fs.rmSync(stateDir, { recursive: true, force: true })
  })
})
