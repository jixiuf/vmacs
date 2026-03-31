#!/usr/bin/env bash

WINDOW_TITLE=${WINDOW_TITLE:-}
WINDOW_CLASS=${WINDOW_CLASS:-}

cwd="$1"

send_txt(){
    autotype.sh --type "$1"  --key enter
}
regex="(\/ssh:)?([a-zA-Z0-9_\-]+@)?([a-zA-Z0-9_\.\-]+):(.+)"
# root@host#2222:/path # host#2222:/path
regex2="(\/ssh:)?([a-zA-Z0-9_]+@)?([a-zA-Z0-9_\.\-]+)#([0-9]+):(.+)"
autotype.sh --key ctrl+u
if [[ $cwd =~ $regex ]]; then
    userat=${BASH_REMATCH[2]}
    host=${BASH_REMATCH[3]}
    path=${BASH_REMATCH[4]}
    termcwd=$(cwd)
    if [[ $termcwd =~ $regex ]]; then
        send_txt "cd $path"
    else
        send_txt " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'"
    fi
elif [[ $cwd =~ $regex2 ]]; then
    userat=${BASH_REMATCH[2]}
    host=${BASH_REMATCH[3]}
    port=${BASH_REMATCH[4]}
    path=${BASH_REMATCH[5]}
    cmd="ssh -t $userat$host -p $port \"cd $path && exec "'\$SHELL'"\" && exec $SHELL"
    termcwd=$(cwd)
    if [[ $termcwd =~ $regex2 ]]; then
        send_txt "cd $path"
    else
        send_txt " ssh -t $userat$host -p $port  'cd $path && exec "'$SHELL'"'"
    fi
else
    send_txt "cd $cwd"
fi

# ;; listen_on unix:/tmp/mykitty
# ;; allow_remote_control yes
# kitty @ --to=unix:/tmp/mykitty-${target_pid} send-key ctrl+u
# kitty @ --to=unix:/tmp/mykitty-${target_pid} send-text " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'\n"
