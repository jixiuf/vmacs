#!/usr/bin/env bash
# support auto type cd with tmux,dotool
PROG=$( basename "$0" )
tmux_session=""
# tmux,dotoolc,dotool
auto_type_cmd=${auto_type_cmd:-}

TEMP=$( getopt --options h --longoptions auto-type:,help -- "$@" ) || exit 1
eval set -- "$TEMP"
for i in "$@"; do
    case "$i" in
        -h|--help)
            echo "Usage: $PROG OPTIONS path"
            echo
            echo
            echo "OPTIONS"
            echo "--tmux-session the tmux session name"
            echo "--auto-type auto-type-cmd "
            echo "  support : dotool,dotoolc,tmux,tmux send-keys"
            echo "  when using tmux, the value can be tmux send-keys or tmux send-keys -t tmux_session"|fmt
            exit 0
            ;;
        --auto-type*)
            auto_type_cmd="$2"
            shift
            shift
            ;;
    esac
done

shift # remove --

cwd="$1"

# guess auto type cmd if empty
if [ -z "$auto_type_cmd" ]; then
    if  pgrep -x "dotoold" > /dev/null ; then
        auto_type_cmd="dotoolc"
    elif [ hash dotool 2>/dev/null ]; then
        auto_type_cmd="dotool"
    fi
fi


if [ "$auto_type_cmd" = "tmux" ]; then
    auto_type_cmd="tmux send-keys"
elif [ -z "$auto_type_cmd" ]; then
    notify-send "you should install dotool to support auto insert: cd $cwd"
    exit
fi
send_text(){
    if [[ "$auto_type_cmd" = "tmux"* ]]; then # use tmux
        $auto_type_cmd "$1"
    elif [[ "$auto_type_cmd" = "dotool"* ]] ; then
         wl-copy  --primary "$1"
         echo key shift+insert |$auto_type_cmd
    fi
}

send_enter(){
    if [[ "$auto_type_cmd" = "tmux"* ]]; then # use tmux
        $auto_type_cmd Enter
    elif [[ "$auto_type_cmd" = "dotool"* ]]; then
         echo key enter |$auto_type_cmd
    fi
}
send_ctrl_u(){
    if [[ "$auto_type_cmd" = "tmux"* ]]; then # use tmux
        $auto_type_cmd C-u
    elif [[ "$auto_type_cmd" = "dotool"* ]]; then
         echo key ctrl+u |$auto_type_cmd
    fi
}

regex="(\/ssh:)?([a-zA-Z0-9_\-]+@)?([a-zA-Z0-9_\.\-]+):(.+)"
# root@host#2222:/path # host#2222:/path
# regex2="(\/ssh:)?([a-zA-Z0-9_]+@)?([a-zA-Z0-9_\.]+)#([0-9]+):(.+)"

# ;; listen_on unix:/tmp/mykitty
# ;; allow_remote_control yes
# kitty @ --to=unix:/tmp/mykitty-${target_pid} send-key ctrl+u
send_ctrl_u
if [[ $cwd =~ $regex ]]; then
    userat=${BASH_REMATCH[2]}
    host=${BASH_REMATCH[3]}
    path=${BASH_REMATCH[4]}
    termcwd=$(cwd)
    # alacritty    -e ssh -t root@bench1 'cd /tmp&& exec $SHELL'
    if [[ $termcwd =~ $regex ]]; then
        # wl-copy  --primary " cd $path"
        # kitty @ --to=unix:/tmp/mykitty-${target_pid} send-text "cd $path\n"
        send_text "cd $path"
        send_enter
    else
        # wl-copy  --primary " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'"
        send_text " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'"
        send_enter
        # kitty @ --to=unix:/tmp/mykitty-${target_pid} send-text " ssh -t $userat$host  'cd $path && exec "'$SHELL'"'\n"
    fi
else
    send_text "cd $cwd"
    send_enter
    # kitty @ --to=unix:/tmp/mykitty-${target_pid} send-text "cd $cwd\n"
    # wl-copy  --primary " cd $cwd"
fi

