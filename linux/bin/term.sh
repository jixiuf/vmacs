#!/bin/bash
# alacritty的简单包装,，当 --working-directory=/ssh:root@host:/path时 将其转为
# term -e ssh -t root@host cd /path&& exec $SHELL
# 即 让alacritty 的--working-directory 支持emacs 的tramp 语法
#!/bin/bash

# term=alacritty
# termexec="-e"
# working_directory_arg="--working-directory"
term=foot
termexec=""
working_directory_arg="--working-directory"
class_arg="--app-id"


# private variable
working_directory=""
class=""
other_args=()
termargs=""

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
    termargs=" $termargs $class_arg=$class"
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
  $term $termargs $other_args $termexec "--" $SHELL -i -c \'$cmd\'
elif [[ $working_directory =~ $regex2 ]]; then
  userat=${BASH_REMATCH[2]}
  host=${BASH_REMATCH[3]}
  port=${BASH_REMATCH[4]}
  path=${BASH_REMATCH[5]}
  cmd="ssh -t $userat$host -p $port \"cd $path && exec "'\$SHELL'"\" && exec $SHELL"
  $term $termargs $other_args $termexec "--" $SHELL -i -c \'$cmd\'
else
    if [ -n "$working_directory" ]; then
        termargs=" $termargs $working_directory_arg=$working_directory"
    fi
    $term $termargs $other_args
        # $term  $other_args
fi
