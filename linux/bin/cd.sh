#!/usr/bin/env bash
# cd.sh  path target_tmux_session

cwd="$1"
tmux_session="$2"

regex="(\/ssh:)?([a-zA-Z0-9_\-]+@)?([a-zA-Z0-9_\.\-]+):(.+)"
sendkey="tmux send-keys"
if [ -n "$tmux_session" ]; then
    sendkey="$sendkey -t $tmux_session"
fi
# root@host#2222:/path # host#2222:/path
# regex2="(\/ssh:)?([a-zA-Z0-9_]+@)?([a-zA-Z0-9_\.]+)#([0-9]+):(.+)"

# ;; listen_on unix:/tmp/mykitty
# ;; allow_remote_control yes
# kitty @ --to=unix:/tmp/mykitty-${target_pid} send-key ctrl+u
$sendkey C-u
if [[ $cwd =~ $regex ]]; then
    userat=${BASH_REMATCH[2]}
    host=${BASH_REMATCH[3]}
    path=${BASH_REMATCH[4]}
    termcwd=$(hypr-cwd)
    # alacritty    -e ssh -t root@bench1 'cd /tmp&& exec $SHELL'
    if [[ $termcwd =~ $regex ]]; then
        # wl-copy  --primary " cd $path"
        # kitty @ --to=unix:/tmp/mykitty-${target_pid} send-text "cd $path\n"
        $sendkey "cd $path"
        $sendkey Enter

    else
        # wl-copy  --primary " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'"
        $sendkey " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'"
        $sendkey Enter
        # kitty @ --to=unix:/tmp/mykitty-${target_pid} send-text " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'\n"
    fi
else
    $sendkey "cd $cwd"
    $sendkey Enter
    # kitty @ --to=unix:/tmp/mykitty-${target_pid} send-text "cd $cwd\n"
    # wl-copy  --primary " cd $cwd"
fi
