#!/usr/bin/env bash

window_title=${window_title:-}

cwd="$1"

send_txt(){
    if [[ $window_title == TMUX:* ]]; then
        autotype.sh --type "$1"  --key enter
    else
        # shift+insert paste primary
        autotype.sh --primary "$1" --key shift+insert --key enter
    fi
}
regex="(\/ssh:)?([a-zA-Z0-9_\-]+@)?([a-zA-Z0-9_\.\-]+):(.+)"
# root@host#2222:/path # host#2222:/path
# regex2="(\/ssh:)?([a-zA-Z0-9_]+@)?([a-zA-Z0-9_\.]+)#([0-9]+):(.+)"

# ;; listen_on unix:/tmp/mykitty
# ;; allow_remote_control yes
# kitty @ --to=unix:/tmp/mykitty-${target_pid} send-key ctrl+u
autotype.sh --key ctrl+u
if [[ $cwd =~ $regex ]]; then
    userat=${BASH_REMATCH[2]}
    host=${BASH_REMATCH[3]}
    path=${BASH_REMATCH[4]}
    termcwd=$(cwd)
    # alacritty    -e ssh -t root@bench1 'cd /tmp&& exec $SHELL'
    if [[ $termcwd =~ $regex ]]; then
        send_txt "cd $path"
    else
        # wl-copy  --primary " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'"
        send_txt " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'"
        # kitty @ --to=unix:/tmp/mykitty-${target_pid} send-text " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'\n"
    fi
else
        send_txt "cd $cwd"
fi

