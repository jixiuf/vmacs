#!/usr/bin/env bash

WINDOW_TITLE=${WINDOW_TITLE:-}
WINDOW_CLASS=${WINDOW_CLASS:-}

# Get cwd: prefer $1, fallback to cwd script (which uses SOURCE_APP/SOURCE_TITLE)
# demo export PATH=/usr/local/bin:$PATH; /usr/local/bin/run-or-raise --toggle  --exec alacritty --title emacscwd --post-cmd /usr/local/bin/cd.sh --cmd /usr/local/bin/term.sh --working-directory=$(cwd||echo $HOME) --tmux-session emacscwd
cwd="$1"
if [ -z "$cwd" ]; then
    cwd=$(cwd 2>/dev/null)
fi

# If cwd is still empty, exit early
if [ -z "$cwd" ]; then
    exit 0
fi

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
