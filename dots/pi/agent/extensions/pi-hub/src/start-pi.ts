// ============================================================================
// 启动 pi：在目标实例所在机器的 tmux 中复用会话开窗口运行 pi
// 从 index.ts 抽出（纯逻辑 + 依赖注入），index.ts 保持装配职责
// ============================================================================

import { execFile } from 'node:child_process'
import * as os from 'node:os'
import type { InstanceInfo, RemoteHostConfig } from './types.js'

export interface ExecResult {
  ok: boolean
  stdout: string
  stderr: string
}

export interface StartPiDeps {
  /** remoteHosts 配置（实例名 → {target, port}） */
  remoteHosts: Record<string, RemoteHostConfig>
  /** 远程执行命令（ssh），供远程 tmux 探测/开窗 */
  sshExec: (target: string, port: number | undefined, command: string, timeoutMs?: number) => Promise<string>
}

export function execCapture(file: string, args: string[], timeoutMs = 20000): Promise<ExecResult> {
  return new Promise((resolve) => {
    execFile(file, args, { timeout: timeoutMs, maxBuffer: 64 * 1024 }, (err, stdout, stderr) => {
      if (err) {
        resolve({ ok: false, stdout: stdout ?? '', stderr: stderr?.toString() ?? (err as Error).message })
        return
      }
      resolve({ ok: true, stdout: stdout ?? '', stderr: stderr?.toString() ?? '' })
    })
  })
}

/** 写入系统剪贴板：macOS pbcopy / Linux xclip / Windows clip */
export function writeClipboard(text: string): Promise<boolean> {
  return new Promise((resolve) => {
    const cmd =
      process.platform === 'darwin' ? 'pbcopy'
      : process.platform === 'linux' ? 'xclip'
      : process.platform === 'win32' ? 'clip' : null
    if (!cmd) { resolve(false); return }
    try {
      const child = execFile(cmd, process.platform === 'linux' ? ['-selection', 'clipboard'] : [], (err) => resolve(!err))
      child.stdin?.write(text)
      child.stdin?.end()
    } catch {
      resolve(false)
    }
  })
}

/** 目录/路径安全校验：拒绝 shell 元字符，杜绝注入（远程经 shell 执行） */
export function isSafePath(p: string): boolean {
  return !/[;&|`'"$()<>*?\[\]{}#\\\n\r]/.test(p)
}

export async function doStartPi(target: InstanceInfo, cwd: string | undefined, deps: StartPiDeps): Promise<string> {
  // 参数安全校验：实例名只允许安全字符，目录拒绝 shell 元字符（远程=shell 执行，必须防注入）
  if (!/^[A-Za-z0-9._-]+$/.test(target.name)) {
    return `❌ 实例名 ${target.name} 含不安全字符，拒绝启动`
  }
  const dir = cwd?.trim() || target.cwd?.trim() || '~'
  if (!isSafePath(dir)) {
    return `❌ 启动目录含不安全字符，拒绝启动: ${dir}`
  }
  // 复用实例所在的 tmux 会话（不新建会话）：窗口名带唯一后缀（实例 pid，未知时回退时间戳）
  const pidSuffix = target.pid > 0 ? String(target.pid) : String(Date.now()).slice(-6)
  const winName = `pi-${target.name}-${pidSuffix}`
  // 实例名确定化：PI_INSTANCE_NAME=<name>-<pid>，避免 start-pi 起的子代理注册名与
  // 协调中心/其他实例冲突（同名存活时 registerInstance 会改协调中心的名字——名字漂移）
  const instName = target.name.endsWith(`-${pidSuffix}`) ? target.name : `${target.name}-${pidSuffix}`
  const isLocal = !target.host || target.host === os.hostname()
  const hostLabel = isLocal ? `${os.hostname()}（本机）` : target.host

  // 1) 定位当前 tmux 会话（复用，绝不新建）
  let sessionName: string
  let sessionTarget: string
  let remote: RemoteHostConfig | undefined
  if (!isLocal) remote = deps.remoteHosts?.[target.name] ?? { target: target.host as string }
  try {
    if (isLocal) {
      if (!process.env.TMUX) {
        // 非 tmux 直接启动（macOS 开 Terminal / Linux nohup），无需 tmux 会话
        return await startDirectLocal(dir, winName, instName)
      }
      sessionName = (await execCapture('tmux', ['display-message', '-p', '#S'])).stdout.trim()
      // 用 session id（如 $1）作 new-window 目标，避免纯数字会话名被当成窗口索引
      sessionTarget = (await execCapture('tmux', ['display-message', '-p', '#{session_id}'])).stdout.trim()
      if (!sessionName || !sessionTarget) return `❌ ${hostLabel} 无法获取当前 tmux 会话`
    } else {
      if (!target.pid || target.pid <= 0) return `❌ 实例 ${target.name} pid 未知，无法定位其 tmux 会话`
      const probe = [
        `SID=$(tr '\\0' '\\n' < /proc/${target.pid}/environ 2>/dev/null | sed -n 's/^TMUX=.*,\\([0-9][0-9]*\\)$/\\1/p' | head -1)`,
        `if [ -z "$SID" ]; then echo 'NO_TMUX'; exit 0; fi`,
        `tmux list-sessions -F '#{session_id} #{session_name}' | awk -v id="\\$$SID" '$1==id{print $1, $2; exit}'`,
      ].join('\n')
      const probeOut = (await deps.sshExec(remote!.target, remote!.port, probe)).trim()
      const [idPart, namePart] = probeOut.split(/\s+/, 2)
      if (!idPart || idPart === 'NO_TMUX') return `❌ 实例 ${target.name} 不在 tmux 会话中（或会话已不存在）`
      sessionTarget = idPart
      sessionName = namePart || target.name
    }
  } catch (err) {
    return `❌ 启动失败（${hostLabel}）: ${(err as Error).message}`
  }

  // 2) 在复用的会话中开窗口
  let status: string
  try {
    if (isLocal) {
      // 本机：execFile 数组参数（不经 shell），窗口去重后创建
      const listRes = await execCapture('tmux', ['list-windows', '-t', sessionTarget, '-F', '#W'])
      if (listRes.ok && listRes.stdout.split('\n').some((w) => w.trim() === winName)) {
        status = 'TMUX_EXISTS'
      } else {
        // env 程序设置 PI_INSTANCE_NAME 后执行 pi（execFile 不经 shell，变量赋值前缀不生效）
        const createRes = await execCapture('tmux', ['new-window', '-t', sessionTarget, '-n', winName, '-c', dir, 'env', `PI_INSTANCE_NAME=${instName}`, 'pi'])
        status = createRes.ok ? 'TMUX_STARTED' : `TMUX_FAILED ${createRes.stderr.trim()}`
      }
    } else {
      const shellCmd = [
        `if tmux list-windows -t '${sessionTarget}' -F '#W' 2>/dev/null | grep -qx '${winName}'; then`,
        `  echo 'TMUX_EXISTS'`,
        `else`,
        `  tmux new-window -t '${sessionTarget}' -n '${winName}' -c '${dir}' "PI_INSTANCE_NAME=${instName} pi" && echo 'TMUX_STARTED' || echo 'TMUX_FAILED'`,
        `fi`,
      ].join('\n')
      status = (await deps.sshExec(remote!.target, remote!.port, shellCmd)).trim().split('\n').pop() ?? ''
    }
  } catch (err) {
    return `❌ 启动失败（${hostLabel}）: ${(err as Error).message}`
  }

  if (status.includes('TMUX_EXISTS')) {
    return `⚠️ 窗口 ${winName} 已存在（${hostLabel}，tmux 会话 ${sessionName}），未重复启动。查看: tmux attach -t ${sessionName}`
  }
  if (status.includes('TMUX_STARTED')) {
    return `✅ 已在 ${hostLabel} 启动 pi，tmux 会话 ${sessionName} 窗口 ${winName}（目录 ${dir}）。查看: tmux attach -t ${sessionName}`
  }
  if (status.includes('DIRECT_STARTED')) {
    return `✅ 已在 ${hostLabel} 直接启动 pi（非 tmux：${process.platform === 'darwin' ? '新 Terminal 窗口' : '后台进程'}），标识 ${winName}（目录 ${dir}）`
  }
  return `❌ 启动失败（${hostLabel}）: ${status.replace(/TMUX_FAILED|DIRECT_FAILED/, '').trim() || '执行异常'}`
}

/**
 * 非 tmux 直接启动新 pi：macOS 开 Terminal 窗口；Linux nohup 后台（日志 /tmp/<winName>.log）。
 * dir 已过 isSafePath 校验（拒绝 shell 元字符），注入安全。
 * instName 作为 PI_INSTANCE_NAME 传入（子代理实例名确定化，避免与协调中心冲突）。
 */
async function startDirectLocal(dir: string, winName: string, instName: string): Promise<string> {
  if (process.platform === 'darwin') {
    const script = `tell application "Terminal" to do script "cd '${dir}' && PI_INSTANCE_NAME=${instName} exec pi"`
    const res = await execCapture('osascript', ['-e', script])
    return res.ok ? 'DIRECT_STARTED' : `DIRECT_FAILED ${res.stderr.trim()}`
  }
  if (process.platform === 'linux') {
    const log = `/tmp/${winName}.log`
    const res = await execCapture('bash', ['-lc', `cd '${dir}' && PI_INSTANCE_NAME=${instName} exec pi > '${log}' 2>&1 &`])
    return res.ok ? 'DIRECT_STARTED' : `DIRECT_FAILED ${res.stderr.trim()}`
  }
  return 'DIRECT_FAILED 非 tmux 直接启动仅支持 macOS/Linux'
}
