#!/usr/bin/env bash
# need `dotool` if you do not use tmux (the window title of tmux should be "TMUX:session:path")
# set -g set-titles on
# set -g set-titles-string 'TMUX:#{session_name}:#{pane_title}'

# support auto type text,keys with tmux,dotool
# support dotool key like : super+alt+ctrl+enter

TERM_CLASS_REGEX=${TERM_CLASS_REGEX:-.*wezterm.*|.*foot.*|.*kitty.*|.*Alacritty.*|dterm|bterm}
WINDOW_TITLE=${WINDOW_TITLE:-}  # current window title
WINDOW_CLASS=${WINDOW_CLASS:-}  # current window class
# tmux,dotoolc,dotool
auto_type_cmd=${auto_type_cmd:-}
PROG=$( basename "$0" )
tmux_session=""
cmd=""
cmds=()

TEMP=$( getopt --options h --longoptions sleep:auto-type:,type:,key:,clipboard:,primary:,help -- "$@" ) || exit 1
eval set -- "$TEMP"
for i in "$@"; do
    case "$i" in
        -h|--help)
            echo "Usage: $PROG OPTIONS "
            echo
            echo
            echo "OPTIONS"
            echo "--auto-type auto-type-cmd "
            echo "  support : dotool,dotoolc,tmux,tmux send-keys"
            echo "  when using tmux, the value can be tmux send-keys or tmux send-keys -t tmux_session"|fmt
            echo "--sleep second"
            echo "--clipboard text"
            echo "  copy text to clipboard"
            echo "--primary text"
            echo "  copy text to primary"
            echo "--type text"
            echo "  type text (do not support utf-8)"
            echo "--key key"
            echo "  send key"
            echo "  demo:"
            echo "    $PROG --key  enter"
            echo "    $PROG --key  ctrl+enter"
            echo "    $PROG --key  ctrl+enter --key shift+1"
            echo "  sending text to terminal by primary"
            echo "    $PROG --primary hello --sleep 0.3 --key shift+insert"

            exit 0
            ;;
        --auto-type*)
            auto_type_cmd="$2"
            shift
            shift
            ;;
        --sleep*)
            cmds+=("sleep:$2")
            shift
            shift
            ;;
        --type*)
            cmds+=("type:$2")
            shift
            shift
            ;;
        --key*)
            cmds+=("key:$2")
            shift
            shift
            ;;
        --clipboard*)
            cmds+=("clipboard:$2")
            shift
            shift
            ;;
        --primary*)
            cmds+=("primary:$2")
            shift
            shift
            ;;
    esac
done

shift # remove --


# guess auto type cmd if empty
if [ -z "$auto_type_cmd" ]; then
    if [ -z "$WINDOW_TITLE" ]; then
        if [ "$XDG_SESSION_DESKTOP"  = "Hyprland" ]; then
            WINDOW_TITLE=`hyprctl activewindow -j |jq -rc ".title"`
        elif [ "$XDG_SESSION_DESKTOP"  = "niri" ]; then
            WINDOW_TITLE=`niri msg -j focused-window|jq -rc .title`
        elif [ "$XDG_SESSION_DESKTOP"  = "sway" ]; then
            WINDOW_TITLE=`swaymsg -t get_tree | jq -rc 'recurse(.nodes[], .floating_nodes[]) |select(.focused)|.name'`
        fi
    fi
    if [ -z "$WINDOW_CLASS" ]; then
        if [ "$XDG_SESSION_DESKTOP"  = "Hyprland" ]; then
            WINDOW_CLASS=`hyprctl activewindow -j |jq -rc '.class'`
        elif [ "$XDG_SESSION_DESKTOP"  = "niri" ]; then
            WINDOW_TITLE=`niri msg -j focused-window|jq -rc .app_id`
        elif [ "$XDG_SESSION_DESKTOP"  = "sway" ]; then
            WINDOW_CLASS=`swaymsg -t get_tree | jq -rc 'recurse(.nodes[], .floating_nodes[]) |select(.focused)|(.app_id // .window_properties.class // "")'|head -n 1`
        fi
    fi
    if [[ $WINDOW_TITLE == TMUX:* ]]; then
        # Remove the prefix "TMUX:" and then split the remaining string by ":"
        # 格式： "TMUX:session:path"
        # set -g set-titles on
        # set -g set-titles-string 'TMUX:#{session_name}:#{pane_title}'
        WINDOW_TITLE=${WINDOW_TITLE#TMUX:}
        tmux_session=${WINDOW_TITLE%%:*}
        auto_type_cmd="tmux send-keys -t $tmux_session"
    elif  pgrep -x "dotoold" > /dev/null ; then
        auto_type_cmd="dotoolc"
    elif [ hash dotool 2>/dev/null ]; then
        auto_type_cmd="dotool"
    fi
fi


if [ "$auto_type_cmd" = "tmux" ]; then
    auto_type_cmd="tmux send-keys"
elif [ -z "$auto_type_cmd" ]; then
    notify-send "you should install dotool to support auto insert: cd $cwd"
    exit 1
fi
copy_clipboard(){
    wl-copy  "$1"
}
copy_primary(){
    wl-copy --primary "$1"
}
send_text(){
    if [[ "$auto_type_cmd" = "tmux"* ]]; then # use tmux
        $auto_type_cmd "$1"
    elif [[ "$WINDOW_CLASS" =~ ^($TERM_CLASS_REGEX)$ ]]; then
        copy_primary "$1"
        send_key shift+insert
    elif [[ "$auto_type_cmd" = "dotool"* ]] ; then
         echo type $1 |$auto_type_cmd
    fi
}
send_key(){
    if [[ "$auto_type_cmd" = "tmux"* ]]; then # use tmux
        key=`echo "$1" | sed 's/ctrl+/C-/g; s/shift+/S-/g; s/alt+/M-/g; s/super+/Super-/g'`
        $auto_type_cmd $key
    elif [[ "$auto_type_cmd" = "dotool"* ]]; then
         echo key $1 |$auto_type_cmd
    fi
}
send_enter(){
    if [[ "$auto_type_cmd" = "tmux"* ]]; then # use tmux
        $auto_type_cmd enter
    elif [[ "$auto_type_cmd" = "dotool"* ]]; then
         echo key enter |$auto_type_cmd
    fi
}


for cmd in "${cmds[@]}"; do
    if [[ $cmd == sleep:* ]]; then
        cmd=${cmd#sleep:}
        sleep $cmd
    elif [[ $cmd == clipboard:* ]]; then
        cmd=${cmd#clipboard:}
        copy_clipboard  "$cmd"
    elif [[ $cmd == primary:* ]]; then
        cmd=${cmd#primary:}
        copy_primary "$cmd"
    elif [[ $cmd == key:* ]]; then
        cmd=${cmd#key:}
        send_key "$cmd"
    elif [[ $cmd == type:* ]]; then
        cmd=${cmd#type:}
        send_text "$cmd"
    fi
done
