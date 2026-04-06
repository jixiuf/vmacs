#!/usr/bin/env bash
# need `dotool` if you do not use tmux (the window title of tmux should be "TMUX:session:path")
# set -g set-titles on
# set -g set-titles-string 'TMUX:#{session_name}:#{pane_title}'

# support auto type text,keys with tmux,dotool
# support dotool key like : super+alt+ctrl+enter

TERM_CLASS_REGEX=${TERM_CLASS_REGEX:-.*wezterm.*|.*foot.*|.*kitty.*|.*Alacritty.*|dterm|bterm|APMSSH|sshemacs|alacritty}
# 用于判断 当前窗口是还是是terminal 或 tmux,用于从title取 tmux session name 等信息
TARGET_TITLE=${TARGET_TITLE:-}  # current window title
TARGET_APP=${TARGET_APP:-}  # current window class
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
    if [ -z "$TARGET_TITLE" ]; then
        if [ "$XDG_SESSION_DESKTOP"  = "Hyprland" ]; then
            TARGET_TITLE=`hyprctl activewindow -j |jq -rc ".title"`
        elif [ "$XDG_SESSION_DESKTOP"  = "niri" ]; then
            TARGET_TITLE=`niri msg -j focused-window|jq -rc .title`
        elif [ "$XDG_SESSION_DESKTOP"  = "sway" ]; then
            TARGET_TITLE=`swaymsg -t get_tree | jq -rc 'recurse(.nodes[], .floating_nodes[]) |select(.focused)|.name'`
        elif [ "$XDG_SESSION_DESKTOP"  = "ewm" ]; then
            TARGET_TITLE=`emacsclient --eval "(ewm-get-window-info-json)"|jq -r|jq -r '.title`
        elif [ "$XDG_SESSION_DESKTOP"  = "reka" ]; then
            TARGET_TITLE=`emacsclient --eval "(reka-get-window-info-json)"|jq -r|jq -r '.title'`
        elif [ "$(uname)" = "Darwin" ]; then
            TARGET_TITLE=$(osascript -e 'tell application "System Events" to tell (first application process whose frontmost is true) to get value of attribute "AXTitle" of front window' 2>/dev/null)
        fi
    fi

    if [ -z "$TARGET_APP" ]; then
        if [ "$XDG_SESSION_DESKTOP"  = "Hyprland" ]; then
            TARGET_APP=`hyprctl activewindow -j |jq -rc '.class'`
        elif [ "$XDG_SESSION_DESKTOP"  = "niri" ]; then
            TARGET_TITLE=`niri msg -j focused-window|jq -rc .app_id`
        elif [ "$XDG_SESSION_DESKTOP"  = "sway" ]; then
            TARGET_APP=`swaymsg -t get_tree | jq -rc 'recurse(.nodes[], .floating_nodes[]) |select(.focused)|(.app_id // .window_properties.class // "")'|head -n 1`
        elif [ "$XDG_SESSION_DESKTOP"  = "ewm" ]; then
            TARGET_APP=`emacsclient --eval "(ewm-get-window-info-json)"|jq -r|jq -r '.app`
        elif [ "$XDG_SESSION_DESKTOP"  = "reka" ]; then
            TARGET_APP=`emacsclient --eval "(reka-get-window-info-json)"|jq -r|jq -r '.app'`
        elif [ "$(uname)" = "Darwin" ]; then
            TARGET_APP=$(osascript -e 'tell application "System Events" to get name of (first application process whose frontmost is true)' 2>/dev/null)
        fi
    fi
    if [[ $TARGET_TITLE == TMUX:* ]]; then
        # Remove the prefix "TMUX:" and then split the remaining string by ":"
        # 格式： "TMUX:session:path"
        # set -g set-titles on
        # set -g set-titles-string 'TMUX:#{session_name}:#{pane_title}'
        TARGET_TITLE=${TARGET_TITLE#TMUX:}
        tmux_session=${TARGET_TITLE%%:*}
        auto_type_cmd="tmux send-keys -t $tmux_session"
    elif  pgrep -x "dotoold" > /dev/null ; then
        auto_type_cmd="dotoolc"
    elif [ hash dotool 2>/dev/null ]; then
        auto_type_cmd="dotool"
    elif [ "$(uname)" = "Darwin" ]; then
        auto_type_cmd="/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"
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
    elif [[ "$TARGET_APP" =~ ^($TERM_CLASS_REGEX)$ ]]; then
        copy_primary "$1"
        send_key shift+insert
    elif [[ "$auto_type_cmd" = "dotool"* ]] ; then
         echo type $1 |$auto_type_cmd
    elif [ "$(uname)" = "Darwin" ]; then
        /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c  "hs.eventtap.keyStrokes(\"$1\")"
    fi
}
send_key(){
    if [[ "$auto_type_cmd" = "tmux"* ]]; then # use tmux
        key=`echo "$1" | sed 's/ctrl+/C-/g; s/shift+/S-/g; s/alt+/M-/g; s/super+/Super-/g'`
        $auto_type_cmd $key
    elif [[ "$auto_type_cmd" = "dotool"* ]]; then
         echo key $1 |$auto_type_cmd
    elif [ "$(uname)" = "Darwin" ]; then
        # Parse modifiers and key for Hammerspoon
        local input="$1"
        local modifiers=""
        local key=""

        # Extract key (last part after the last +)
        key="${input##*+}"
        # Extract modifiers (everything before the last +)
        if [[ "$input" == *"+"* ]]; then
            modifiers="${input%+*}"
        fi

        # Convert modifier names to Hammerspoon format
        local hs_modifiers=""
        local mod_array=()
        IFS='+' read -ra MODS <<< "$modifiers"
        for mod in "${MODS[@]}"; do
            case "$mod" in
                ctrl)  mod_array+=('"cmd"') ;;
                shift) mod_array+=('"shift"') ;;
                alt)   mod_array+=('"alt"') ;;
                super) mod_array+=('"cmd"') ;;
            esac
        done

        # Join modifiers with commas
        if [ ${#mod_array[@]} -gt 0 ]; then
            hs_modifiers=$(IFS=,; echo "${mod_array[*]}")
        fi

        # Convert key names to Hammerspoon format
        case "$key" in
            enter|return) key="return" ;;
            escape|esc) key="escape" ;;
            tab) key="tab" ;;
            space) key="space" ;;
            backspace|bs) key="delete" ;;
            delete|del) key="forwarddelete" ;;
            insert) key="help" ;;
            home) key="home" ;;
            end) key="end" ;;
            pageup|pgup) key="pageup" ;;
            pagedown|pgdn) key="pagedown" ;;
            up) key="up" ;;
            down) key="down" ;;
            left) key="left" ;;
            right) key="right" ;;
            f1|f2|f3|f4|f5|f6|f7|f8|f9|f10|f11|f12) ;; # F-keys stay the same
            *) key="$key" ;; # For regular keys, use as-is
        esac

        local lua_cmd="hs.eventtap.keyStroke({${hs_modifiers}}, '${key}')"
        /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c "$lua_cmd"
    fi
}
send_enter(){
    if [[ "$auto_type_cmd" = "tmux"* ]]; then # use tmux
        $auto_type_cmd enter
    elif [[ "$auto_type_cmd" = "dotool"* ]]; then
         echo key enter |$auto_type_cmd
    elif [ "$(uname)" = "Darwin" ]; then
        /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c "hs.eventtap.keyStroke({}, 'return')"
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
