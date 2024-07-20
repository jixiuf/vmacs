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

killall wf-record-stop.sh 2>/dev/null

if [[ -z $(pgrep wf-recorder) ]]
then
    mkdir -p "${tmp_dir}"

    filepath="${tmp_dir}/$(date "+%s").mp4"
    echo "$filepath" > "${tmp_file}"
    case $chosen in
        $screen)
            swaync-client -cp
            # pkill -RTMIN+8 waybar
            sleep 1;
            cd /tmp/&&nohup wf-recorder  --audio --file="${filepath}" >/tmp/wf-recorder.log &
            ;;
        $area)
            if command -v slurp >/dev/null 2>&1; then
                swaync-client -cp
                # pkill -RTMIN+8 waybar
                g="$(slurp)"
                sleep 1
                cd /tmp/&&nohup wf-recorder -g "$g"  --audio --file="${filepath}"  >/tmp/wf-recorder.log &
            else
                $notify_cmd_shot  "slurp not found"
            fi
            ;;
        $window)
                swaync-client -cp
                # pkill -RTMIN+8 waybar
                sleep 1
                cd /tmp/&&nohup wf-recorder -g "$(hyprctl activewindow | grep at: | cut -d' ' -f2) $(hyprctl activewindow | grep size: | cut -d' ' -f2 | sed 's/,/x/g')"  --audio --file="${filepath}" >/tmp/wf-recorder.log &
            ;;
    esac
    $notify_cmd_shot "Screen Record" "录屏开始..."
    sleep 2;
    if [ -z $(pgrep wf-recorder) ]; then
        ${notify_cmd_shot} "Screen Record" "没有 录制中的视频!"
    fi
else
    swaync-client -cp
    $notify_cmd_shot "Screen Record" "已有正在进行中的录屏!"
fi
