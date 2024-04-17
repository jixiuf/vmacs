#!/bin/sh

icon_path="$HOME/.config/hypr/icons/video.png"
notify_cmd_shot="notify-send -t 1000  -h string:x-canonical-private-synchronous:screeenrecord -u low -i ${icon_path}"
# action=$($notify_cmd_shot "Screen Record" "Saved to ${saved_to}" --action " Dired" --action "Play" --action "To Gif" --action "To Gif W800")
# if [[ "${action}" == "0" ]]; then
# fi

recordings="$HOME/Videos/recordings"
tmp_dir="${recordings}/.tmp"
tmp_file="${tmp_dir}/.recording.mp4"
linkname="${recordings}/latest.mp4"

if [ ! -z $(pgrep wf-recorder) ]; then
    killall -s SIGINT wf-recorder
    while [ ! -z $(pgrep -x wf-recorder) ]; do wait; done
    pkill -RTMIN+8 waybar

    if [ -f "${tmp_file}" ]; then
        tmp_file="$(cat $tmp_file)"
        filename="record_$(date "+%Y-%m-%d-%H-%M-%S").mp4"
        filepath="${recordings}/${filename}"
        mv "${tmp_file}" "${filepath}"
        ln -sfr ${filepath} ${linkname}
        ${notify_cmd_shot} "Screen Record" "录制结束。"
    fi
else
    ${notify_cmd_shot} "Screen Record" "没有 录制中的视频!"
fi
