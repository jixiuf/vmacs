import { describe, it, expect } from 'vitest'
import {
  isTaskResultText,
  isTerminalStatus,
  isPendingReplyEligible,
  extractTaskId,
  extractReplyText,
  hasManualTaskReply,
} from '../src/task.js'

// 与 index.ts onMessage 登记判定一致的最小模拟（纯逻辑提取，不依赖扩展闭包）：
// 一条消息若【不含任务 ID】或【来自自身】则不登记；否则由 isPendingReplyEligible 决定。
function simulateRegister(
  text: string,
  taskStatus: string | undefined,
  from: string,
  currentInstance: string,
): { taskId: string | null; registered: boolean } {
  const taskId = extractTaskId(text)
  if (!taskId || from === currentInstance) return { taskId, registered: false }
  return { taskId, registered: isPendingReplyEligible(text, taskStatus) }
}

// 真实格式样例：
// 分配消息（dispatch_task 生成）：以 [TASK#N] 开头，正文含任务 ID 与回传协议说明
const ASSIGN_MSG =
  '[TASK#1] 分析代码\n\n【回传协议（必须执行，否则主实例看不到你的结果）】\n' +
  '你的本会话回复不会被主实例看到。处理完成后必须调用 send_message 工具：\n' +
  '1. target（目标实例）: home\n' +
  '2. text（消息内容）: [TASK-1788425681668-ot14结果] + JSON，示例：{"status":"done","data":...}\n' +
  '任务ID: TASK-1788425681668-ot14（回传时保留，主实例自动识别并更新状态）。'

// 结果消息（自动回传 / 手动 send_message 产物）：以 [TASK-xxx结果] 开头
const RESULT_MSG = '[TASK-1788425681668-ot14结果] {"status":"done","data":{"ok":true}}'

describe('isTaskResultText', () => {
  it('结果消息（[TASK-\d+-…结果] 开头）识别为 true', () => {
    expect(isTaskResultText(RESULT_MSG)).toBe(true)
    expect(isTaskResultText('[TASK-123-abc结果] 已完成')).toBe(true)
    expect(isTaskResultText('[TASK#7结果] {"status":"failed"}')).toBe(true)
  })

  it('任务分配消息（[TASK#N] 开头，正文含回传协议）不被误判为结果', () => {
    // 分配消息正文里出现了「[TASK-…结果]」字样，但并非以结果标记开头→不误判
    expect(isTaskResultText(ASSIGN_MSG)).toBe(false)
    expect(isTaskResultText('[TASK#1] 分析代码')).toBe(false)
  })

  it('普通文本 / 空串识别为 false', () => {
    expect(isTaskResultText('普通回复，无任务标记')).toBe(false)
    expect(isTaskResultText('')).toBe(false)
  })
})

describe('isTerminalStatus', () => {
  it('done/failed/timeout 为终结，pending/running/undefined 非终结', () => {
    expect(isTerminalStatus('done')).toBe(true)
    expect(isTerminalStatus('failed')).toBe(true)
    expect(isTerminalStatus('timeout')).toBe(true)
    expect(isTerminalStatus('pending')).toBe(false)
    expect(isTerminalStatus('running')).toBe(false)
    expect(isTerminalStatus(undefined)).toBe(false)
  })
})

describe('isPendingReplyEligible（结果消息/终结任务不登记）', () => {
  it('结果消息不登记（防 [TASK-x结果] 回流）', () => {
    expect(isPendingReplyEligible(RESULT_MSG, undefined)).toBe(false)
    expect(isPendingReplyEligible(RESULT_MSG, 'pending')).toBe(false)
  })

  it('已终结任务的消息不登记（done/failed/timeout）', () => {
    const text = '[TASK#2] 收尾任务 任务ID: TASK-999-xyz'
    expect(isPendingReplyEligible(text, 'done')).toBe(false)
    expect(isPendingReplyEligible(text, 'failed')).toBe(false)
    expect(isPendingReplyEligible(text, 'timeout')).toBe(false)
  })

  it('普通任务分配消息（未终结 / 无状态）可登记', () => {
    expect(isPendingReplyEligible(ASSIGN_MSG, 'pending')).toBe(true)
    expect(isPendingReplyEligible(ASSIGN_MSG, undefined)).toBe(true)
  })
})

describe('自动回传死循环（ping-pong）回归', () => {
  const taskId = 'TASK-1788425681668-ot14'
  const assignee = 'B'
  const dispatcher = 'A'

  it('B 收到分配消息可登记（正常派发）', () => {
    const r = simulateRegister(ASSIGN_MSG, undefined, dispatcher, assignee)
    expect(r.taskId).toBe(taskId)
    expect(r.registered).toBe(true)
  })

  it('A 收到 B 的结果消息不再登记（死循环被打断）', () => {
    // A 收到 [TASK-x结果]，此时 tryAutoUpdateTask 已把任务置为 done
    const r = simulateRegister(RESULT_MSG, 'done', assignee, dispatcher)
    expect(r.taskId).toBe(taskId)
    expect(r.registered).toBe(false)
  })

  it('B 收到 A 回流的结果消息也不再登记（反向同样打断）', () => {
    const r = simulateRegister(RESULT_MSG, undefined, dispatcher, assignee)
    expect(r.taskId).toBe(taskId)
    expect(r.registered).toBe(false)
  })

  it('来自自身的消息不登记', () => {
    // 本机实例绕过（env.from === currentInstanceName），避免本地自我投递再登记
    const r = simulateRegister(ASSIGN_MSG, undefined, assignee, assignee)
    expect(r.registered).toBe(false)
  })

  it('结果消息经 extractTaskId 仍能抽到任务 ID（供 tryAutoUpdateTask 更新状态）', () => {
    expect(extractTaskId(RESULT_MSG)).toBe(taskId)
  })
})

describe('任务回传相关纯函数基线', () => {
  it('extractReplyText 正常提取 & hasManualTaskReply 不回归', () => {
    const branch = [
      { type: 'message', message: { role: 'assistant', content: '完成，结果 JSON' } },
    ] as unknown[]
    expect(extractReplyText(branch)).toBe('完成，结果 JSON')
    expect(hasManualTaskReply('[TASK-123-abc结果] 手动', 'TASK-123-abc')).toBe(true)
    expect(hasManualTaskReply('自动回传', 'TASK-123-abc')).toBe(false)
  })
})
