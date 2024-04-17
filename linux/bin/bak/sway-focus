#!/usr/bin/env bash
# https://gitlab.com/wef/dotfiles/-/blob/master/bin/sway-focus
# shellcheck disable=SC2034
TIME_STAMP="20230223.131349"

# Copyright (C) 2020-2021 Bob Hepple <bob dot hepple at gmail dot com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

PROG=$( basename "$0" )
toggle=""
origin=""
verbose="-q" # for swaymsg

TEMP=$( getopt --options sthv --longoptions toggle-scratchpad,send-to-origin,verbose,help -- "$@" ) || exit 1
eval set -- "$TEMP"

for i in "$@"; do
    case "$i" in
        -h|--help)
            echo "Usage: $PROG OPTIONS target [runstring]"
            echo
            echo "Give focus to a program based on target (app_id for sway or class for Xwayland). If we can't give focus to something with that app_id or class, then exec 'runstring'" |fmt
            echo
            echo "OPTIONS"
            echo "-t|--toggle-scratchpad    send the program to/from scratchpad"
            echo "-s|--send-to-origin       also send to 0,0"
            echo "-v|--verbose              be verbose"
            exit 0
            ;;
        -t|--tog*)
            toggle="set"
            shift
            ;;
        -s|--send-to-origin)
            origin="set"
            shift
            ;;
        -v|--verbose)
            verbose=""
            ;;
    esac
done

shift
target="$1"
runstring="$2"

if [[ -z "$verbose" ]]; then
    echo "$PROG: target='$target' runstring='$runstring'" >&2
    set -x
fi

[[ "$toggle" ]] && {
    program_data=$( swaymsg -t get_tree | jq ".. | select(.type?) | select(.app_id==\"$target\" or .window_properties.class==\"$target\")" )
    if [[ "$program_data" ]]; then
        id=$( echo "$program_data" | jq ".id" | head -n 1)
        visible=$( echo "$program_data" | jq ".visible" | head -n 1)
        if [[ "$visible" == "false" ]]; then
            # shellcheck disable=SC2086
            swaymsg $verbose "[con_id=$id] move window to workspace current"
            # shellcheck disable=SC2086
            swaymsg $verbose "[con_id=$id] focus"
            if [[ "$origin" ]]; then
                # shellcheck disable=SC2086
                swaymsg $verbose "[con_id=$id] move position 0 0"
            fi
        else
            # shellcheck disable=SC2086
            swaymsg $verbose "[con_id=$id] move window to scratchpad"
        fi
    else
        if [[ "$runstring" ]]; then
            # shellcheck disable=SC2086
            swaymsg $verbose exec "$runstring"
        fi
    fi
    exit 0
}

# shellcheck disable=SC2086
swaymsg $verbose "[app_id=$target] focus" || {
    # could be Xwayland app:  
    # shellcheck disable=SC2086
    swaymsg $verbose "[class=$target] focus" || {
        [[ "$runstring" ]] || runstring="$target"
            # not running yet
            # shellcheck disable=SC2086
        exec $runstring || exit 0
    }
}

# Local Variables:
# mode: shell-script
# time-stamp-pattern: "4/TIME_STAMP=\"%:y%02m%02d.%02H%02M%02S\""
# eval: (add-hook 'before-save-hook 'time-stamp)
# End:

