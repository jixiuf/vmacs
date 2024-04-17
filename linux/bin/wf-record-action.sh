#!/bin/sh

# icon_path="$HOME/.config/hypr/icons/video.png"
# notify_cmd_shot="notify-send -h string:x-canonical-private-synchronous:screeenrecord -u low -i ${icon_path}"

recordings="$HOME/Videos/recordings"
linkname="${recordings}/latest.mp4"
linkgif="${recordings}/latest.gif"
filepath="$(realpath $linkname)"
action=$1

if [[ "${action}" == "0" ]]; then
    ec -n $recordings
elif [[ "${action}" == "1" ]]; then
    xdg-open $linkname
elif [[ "${action}" == "2" ]]; then
    # 转gif
    gifoutput=$(dirname $filepath)/$(basename $filepath ".mp4").gif
    mp4togif $filepath $gifoutput
    ln -srf $gifoutput $linkgif
    xdg-open $linkgif
elif [[ "${action}" == "3" ]]; then
    # 转gif width=800
    gifoutput=$(dirname $filepath)/$(basename $filepath ".mp4").gif
    mp4togif $filepath $gifoutput 800
    ln -srf $gifoutput $linkgif
    xdg-open $linkgif
fi
