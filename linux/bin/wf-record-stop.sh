#!/bin/sh

icon_path="$HOME/.config/hypr/icons/video.png"
notify_cmd_shot="notify-send -t 1000 -h string:x-canonical-private-synchronous:screeenrecord -u low -i ${icon_path}"

recordings="$HOME/Videos/recordings"
tmp_dir="${recordings}/.tmp"
tmp_file="${tmp_dir}/.recording.mp4"
pid_file="${tmp_dir}/.recording.pid"
linkname="${recordings}/latest.mp4"

# --- Phase 1: try to stop any running wf-recorder ---

rec_pid=""
if [ -f "$pid_file" ]; then
    rec_pid=$(cat "$pid_file")
fi

# If PID file exists and process is alive, stop it gracefully
if [ -n "$rec_pid" ] && kill -0 "$rec_pid" 2>/dev/null; then
    kill -s SIGINT "$rec_pid" 2>/dev/null

    # Wait for wf-recorder to finish writing (max ~10 seconds)
    timeout=20
    while kill -0 "$rec_pid" 2>/dev/null && [ "$timeout" -gt 0 ]; do
        sleep 0.5
        timeout=$((timeout - 1))
    done

    # Force kill if still running
    if kill -0 "$rec_pid" 2>/dev/null; then
        kill -s SIGKILL "$rec_pid" 2>/dev/null
        sleep 0.3
    fi
elif pgrep -x wf-recorder >/dev/null 2>&1; then
    # Fallback: PID file missing but wf-recorder is actually running
    killall -s SIGINT wf-recorder 2>/dev/null

    timeout=20
    while pgrep -x wf-recorder >/dev/null 2>&1 && [ "$timeout" -gt 0 ]; do
        sleep 0.5
        timeout=$((timeout - 1))
    done

    if pgrep -x wf-recorder >/dev/null 2>&1; then
        killall -s SIGKILL wf-recorder 2>/dev/null
        sleep 0.3
    fi
fi

# --- Phase 2: save the recording file (whether wf-recorder was alive or not) ---

saved=false
if [ -f "$tmp_file" ]; then
    src="$(cat "$tmp_file")"
    if [ -f "$src" ]; then
        # Only save if the file has actual content (not just empty header)
        size=$(stat -c%s "$src" 2>/dev/null || echo 0)
        if [ "$size" -gt 1024 ]; then
            filename="record_$(date "+%Y-%m-%d-%H-%M-%S").mp4"
            filepath="${recordings}/${filename}"
            mkdir -p "${recordings}"
            mv "$src" "$filepath"
            ln -sfr "$filepath" "$linkname"
            ${notify_cmd_shot} "Screen Record" "录制结束。已保存至 ${recordings}/${filename}"
            saved=true
        else
            # File too small (probably failed recording), discard it
            rm -f "$src"
            ${notify_cmd_shot} "Screen Record" "录制文件为空，已丢弃"
        fi
    fi
fi

if ! $saved; then
    ${notify_cmd_shot} "Screen Record" "没有 录制中的视频!"
fi

# --- Phase 3: cleanup ---

# Remove metadata files
rm -f "$pid_file" "$tmp_file"

# Clean up orphan temp files older than 1 hour (from crashed sessions)
find "${tmp_dir}" -name "*.mp4" -mmin +60 -delete 2>/dev/null

# Notify waybar to update recording indicator icon
pkill -RTMIN+8 waybar 2>/dev/null
