#!/bin/bash
# session cwd 后面有 ":" 表示后面接参数
PROG=$( basename "$0" )
TEMP=$( getopt --options h --longoptions session:,cwd:,help -- "$@" ) || exit 1
eval set -- "$TEMP"
session_1=""
session_2=""
cwd="$PWD"

for i in "$@"; do
    case "$i" in
        -h|--help)
            echo "Usage: $PROG --session sess_name =-cwd cwd other args"
            exit 0
            ;;
        --session*)
            session_1="-t$2"
            session_2="-s$2"
            shift
            shift
            ;;
        --cwd*)
            cwd="$2"
            shift
            shift
            ;;
    esac
done

shift # remove --
cd $cwd ;tmux attach  $session_1 -c $cwd||tmux new-session $session_2 $@
