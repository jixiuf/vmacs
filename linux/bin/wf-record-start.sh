#!/bin/sh

icon_path="$HOME/.config/hypr/icons/video.png"
notify_cmd_shot="notify-send -t 1000 -h string:x-canonical-private-synchronous:screeenrecord -u low -i ${icon_path}"

recordings="$HOME/Videos/recordings"
tmp_dir="${recordings}/.tmp"
tmp_file="${tmp_dir}/.recording.mp4"
pid_file="${tmp_dir}/.recording.pid"

chosen="$1"

# Detect existing recording: PID file first, then pgrep as fallback
if [ -f "$pid_file" ]; then
    pid=$(cat "$pid_file")
    if kill -0 "$pid" 2>/dev/null; then
        $notify_cmd_shot "Screen Record" "已有正在进行中的录屏!"
        exit 0
    fi
    # Stale PID file — clean up below before starting new recording
fi

# Fallback: pgrep in case PID file is missing but wf-recorder is actually running
if pgrep -x wf-recorder >/dev/null 2>&1; then
    $notify_cmd_shot "Screen Record" "已有正在进行中的录屏!"
    exit 0
fi

# Clean up any stale metadata from previous crashed sessions
rm -f "$pid_file" "$tmp_file"

# Only kill pending stop scripts when we are about to start a new recording
killall wf-record-stop.sh 2>/dev/null

mkdir -p "${tmp_dir}" "${recordings}"
filepath="${tmp_dir}/$(date "+%s").mp4"

# Handle region selection BEFORE writing metadata
# (if slurp is cancelled, nothing is written to disk)
case "$chosen" in
    屏幕)
        ;;
    窗口)
        ;;
    区域)
        if command -v slurp >/dev/null 2>&1; then
            g="$(slurp)"
            if [ -z "$g" ]; then
                $notify_cmd_shot "Screen Record" "区域选择已取消"
                exit 0
            fi
        else
            $notify_cmd_shot "Screen Record" "slurp not found"
            exit 1
        fi
        ;;
    *)
        $notify_cmd_shot "Screen Record" "未知模式: $chosen"
        exit 1
        ;;
esac

# Write file-path metadata BEFORE starting wf-recorder.
# This ensures the stop script always knows where the file is,
# even if wf-recorder crashes immediately.
echo "$filepath" > "$tmp_file"

# Start wf-recorder
case "$chosen" in
    屏幕)
        nohup wf-recorder --audio --file="${filepath}" >/tmp/wf-recorder.log 2>&1 &
        ;;
    窗口)
        geometry="$(hyprctl activewindow | grep at: | cut -d' ' -f2) $(hyprctl activewindow | grep size: | cut -d' ' -f2 | sed 's/,/x/g')"
        nohup wf-recorder -g "$geometry" --audio --file="${filepath}" >/tmp/wf-recorder.log 2>&1 &
        ;;
    区域)
        nohup wf-recorder -g "$g" --audio --file="${filepath}" >/tmp/wf-recorder.log 2>&1 &
        ;;
esac

# Write PID after wf-recorder is started
echo "$!" > "$pid_file"

$notify_cmd_shot "Screen Record" "录屏开始..."
