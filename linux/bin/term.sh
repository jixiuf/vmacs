#!/bin/bash
# make alacritty/foot/kitty --working-directory suppporting emacs tramp path
# alacritty/foot/kitty的简单包装,，当 --working-directory=/ssh:root@host:/path时 将其转为
# term.sh  --working-directory=/tmp
# term.sh  --working-directory=/ssh:root@host:/tmp
# term.sh --class=dterm --working-directory=/ssh:bench:/ 
# term.sh --class=dterm --working-directory=/ssh:bench:/ -- tmux.sh --session dterm --cwd /ssh:bench:/
# term.sh  --working-directory=$cwd -- tmux.sh --session $RANDOM --cwd $(cwd||echo $HOME)
# 即 让alacritty 的--working-directory 支持emacs 的tramp 语法
#!/bin/bash
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
  $TERMINAL $term_args $TERMINAL_EXEC $other_args  "--" $SHELL -i -c \'$cmd\'
elif [[ $working_directory =~ $regex2 ]]; then
  userat=${BASH_REMATCH[2]}
  host=${BASH_REMATCH[3]}
  port=${BASH_REMATCH[4]}
  path=${BASH_REMATCH[5]}
  cmd="ssh -t $userat$host -p $port \"cd $path && exec "'\$SHELL'"\" && exec $SHELL"
  $TERMINAL $term_args $TERMINAL_EXEC $other_args  "--" $SHELL -i -c \'$cmd\'
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
