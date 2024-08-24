#!/usr/bin/env bash
# make alacritty/foot/kitty --working-directory suppporting emacs tramp path
# --working-directory support path like:
# root@host:/path
# root@host#2222:/path
# /ssh:root@host:/path
# /ssh:root#2222@host:/path

# term.sh --working-directory=/tmp
# term.sh --working-directory=root@host:/tmp
# term.sh --working-directory=/ssh:root@host:/tmp
# term.sh --working-directory=/ssh:root@host:/ -- tmux.sh --session dterm --cwd /ssh:root@host:/
# term.sh --class=dterm --working-directory=/ssh:root@bench:/

# term.sh --working-directory=$(cwd||echo $HOME)
# term.sh --working-directory=$(cwd||echo $HOME) -- tmux.sh --session dterm --cwd $(cwd||echo $HOME)

# 即 让alacritty 的--working-directory 支持emacs 的tramp 语法
TERMINAL=${TERMINAL:-alacritty}
if [ "$TERMINAL"  = "alacritty" ]; then
    TERMINAL_EXEC="-e"
    WORKING_DIRECTORY_ARG="--working-directory"
    CLASS_ARG="--class"
elif [ "$TERMINAL"  = "foot" ]; then
    TERMINAL_EXEC=""
    WORKING_DIRECTORY_ARG="--working-directory"
    CLASS_ARG="--app-id"
elif [ "$TERMINAL"  = "kitty" ]; then
    TERMINAL_EXEC=""
    WORKING_DIRECTORY_ARG="--working-directory"
    CLASS_ARG="--app-id"
fi


# private variable
working_directory=""
class=""
other_args=""
term_args=""

PROG=$( basename "$0" )
TEMP=$( getopt --options h --longoptions class:,working-directory:,help -- "$@" ) || exit 1
eval set -- "$TEMP"

for i in "$@"; do
    case "$i" in
        -h|--help)
            echo "Usage: $PROG --class classname --working-directory cwd  other args"
            exit 0
            ;;
        --class*)
            class="$2"
            shift
            shift
            ;;
        --working-directory*)
            working_directory="$2"
            shift
            shift
            ;;
    esac
done
shift # remove --
other_args=$@

if [ -n "$class" ]; then
    term_args=" $term_args $CLASS_ARG=$class"
fi

# root@host:/path or host:/path
regex="(\/ssh:)?([a-zA-Z0-9_\-]+@)?([a-zA-Z0-9_\.\-]+):(.+)"
# root@host#2222:/path # host#2222:/path
regex2="(\/ssh:)?([a-zA-Z0-9_]+@)?([a-zA-Z0-9_\.\-]+)#([0-9]+):(.+)"

if [[ $working_directory =~ $regex ]]; then
  userat=${BASH_REMATCH[2]}
  host=${BASH_REMATCH[3]}
  path=${BASH_REMATCH[4]}
  cmd="ssh -t $userat$host \"cd $path && exec "'\$SHELL'"\" && exec $SHELL"
  if [ -z "$other_args" ]; then
     eval $TERMINAL $term_args $TERMINAL_EXEC  $SHELL  -i -c \'$cmd\'
  else
       $TERMINAL $term_args $TERMINAL_EXEC $other_args  "--" $SHELL -i -c \'$cmd\'
  fi
elif [[ $working_directory =~ $regex2 ]]; then
  userat=${BASH_REMATCH[2]}
  host=${BASH_REMATCH[3]}
  port=${BASH_REMATCH[4]}
  path=${BASH_REMATCH[5]}
  cmd="ssh -t $userat$host -p $port \"cd $path && exec "'\$SHELL'"\" && exec $SHELL"
  if [ -z "$other_args" ]; then
     eval $TERMINAL $term_args $TERMINAL_EXEC  $SHELL  -i -c \'$cmd\'
  else
       $TERMINAL $term_args $TERMINAL_EXEC $other_args  "--" $SHELL -i -c \'$cmd\'
  fi
else
    if [ -n "$working_directory" ]; then
        term_args=" $term_args $WORKING_DIRECTORY_ARG=$working_directory"
    fi
    if [ -z "$other_args" ]; then
        $TERMINAL $term_args
    else
        $TERMINAL $term_args $TERMINAL_EXEC $other_args
    fi
fi
