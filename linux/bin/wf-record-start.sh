#!/bin/sh

icon_path="$HOME/.config/hypr/icons/video.png"
notify_cmd_shot="notify-send -t 1000 -h string:x-canonical-private-synchronous:screeenrecord -u low -i ${icon_path}"

recordings="$HOME/Videos/recordings"
tmp_dir="${recordings}/.tmp"
tmp_file="${tmp_dir}/.recording.mp4"

rofi_command="rofi "

# Options
screen="屏幕"
area="区域"
window="窗口"

# Variable passed to rofi
# options="$screen\n$area\n$window"
chosen=$1

killall wf-record-stop.sh

if [ -z $(pgrep wf-recorder) ]
then
    mkdir -p "${tmp_dir}"

    filepath="${tmp_dir}/$(date "+%s").mp4"
    echo "$filepath" > "${tmp_file}"
    case $chosen in
        $screen)
            sleep 1;wf-recorder  --audio --file="${filepath}" & disown && \
                $notify_cmd_shot "Screen Record" "录屏开始..." && \
                pkill -RTMIN+8 waybar
            ;;
        $area)
            if command -v slurp >/dev/null 2>&1; then
                g="$(slurp)"
                sleep 1;wf-recorder -g "$g"  --audio --file="${filepath}" & disown && \
                    $notify_cmd_shot "Screen Record" "录屏开始..." && \
                    pkill -RTMIN+8 waybar
            else
                $notify_cmd_shot  "slurp not found"
            fi
            ;;
        $window)
            sleep 1; wf-recorder -g "$(hyprctl activewindow | grep at: | cut -d' ' -f2) $(hyprctl activewindow | grep size: | cut -d' ' -f2 | sed 's/,/x/g')"  --audio --file="${filepath}" & disown && \
                $notify_cmd_shot "Screen Record" "录屏开始...." && \
                pkill -RTMIN+8 waybar
            ;;
    esac


else
    $notify_cmd_shot "Screen Record" "已有正在进行中的录屏!"
fi
