#!/usr/bin/env bash

## Author  : Aditya Shakya
## Mail    : adi1090x@gmail.com
## Github  : @adi1090x
## Twitter : @adi1090x

# dir="$HOME/.config/rofi/"
# rofi_command="rofi -theme $dir/global/rofi.rasi"
rofi_command="rofi "

# Options
screen="屏幕"
area="区域"
window="窗口"

# Variable passed to rofi
options="$screen\n$area\n$window"

chosen="$(echo -e "$options" | $rofi_command -p '' -dmenu -selected-row 1)"
case $chosen in
    $screen)
    sleep 0.4; grim -t png - |swappy -f -
        ;;
    $area)
		grim -g "$(slurp)"  -|swappy -f -
        ;;
    $window)
		sleep 1; grim -g "$(hyprctl activewindow | grep at: | cut -d' ' -f2) $(hyprctl activewindow | grep size: | cut -d' ' -f2 | sed 's/,/x/g')" - | swappy -f -
        ;;
esac
