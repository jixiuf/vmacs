#!/bin/bash
# 2026-08-25: 刷新 tmux 的 SSH_AUTH_SOCK 并 attach 会话
# 同时更新: server 全局 + 所有会话环境, 保证新窗口/新pane都拿到当前连接的 socket
SESSION="${1:-vc}"
if [ -n "$SSH_AUTH_SOCK" ] && [ -S "$SSH_AUTH_SOCK" ]; then
    tmux set-environment -g SSH_AUTH_SOCK "$SSH_AUTH_SOCK" 2>/dev/null
    # 更新所有已有会话的环境(覆盖旧死值)
    for s in $(tmux list-sessions -F '#{session_name}' 2>/dev/null); do
        tmux set-environment -t "$s" SSH_AUTH_SOCK "$SSH_AUTH_SOCK" 2>/dev/null
    done
fi
exec tmux new-session -A -s "$SESSION"
