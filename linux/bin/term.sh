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

# term=kitty
# termexec=""
# working_directory_arg="--working-directory"

# term="wezterm start "
# termexec=""
# working_directory_arg="--cwd"

working_directory=""
other_args=()

# 解析参数
for arg in "$@"; do
  if [[ $arg == --working-directory=* ]]; then
    working_directory="${arg#*=}"
  else
    other_args+=("$arg")
  fi
done

# root@host:/path or host:/path
regex="(\/ssh:)?([a-zA-Z0-9_\-]+@)?([a-zA-Z0-9_\.\-]+):(.+)"
# root@host#2222:/path # host#2222:/path
regex2="(\/ssh:)?([a-zA-Z0-9_]+@)?([a-zA-Z0-9_\.\-]+)#([0-9]+):(.+)"

if [[ $working_directory =~ $regex ]]; then
  userat=${BASH_REMATCH[2]}
  host=${BASH_REMATCH[3]}
  path=${BASH_REMATCH[4]}
  # 执行alacritty时移除--working-directory参数，并添加-e ssh ${USER}@${HOST} cd ${Path}&& exec $SHELL
  # kitty    -e ssh -t root@bench1 'cd /tmp&& exec $SHELL'
  # alacritty    -e ssh -t root@bench1 'cd /tmp&& exec $SHELL'
  #
  cmd=("$term" "${other_args[@]}" $termexec  "--" $SHELL -i -c "ssh -t $userat$host \"cd $path && exec "'\$SHELL'"\" &&exec $SHELL")
  "${cmd[@]}"
  # $term $other_args $termexec $SHELL -i -c "ssh -t $userat$host \"cd $path && exec "'\$SHELL'"\" &&exec $SHELL"
elif [[ $working_directory =~ $regex2 ]]; then
  userat=${BASH_REMATCH[2]}
  host=${BASH_REMATCH[3]}
  port=${BASH_REMATCH[4]}
  path=${BASH_REMATCH[5]}
  # 执行alacritty时移除--working-directory参数，并添加-e ssh ${USER}@${HOST} cd ${Path}&& exec $SHELL
  # $term $other_args $termexec $SHELL -i -c "ssh -t $userat$host -p $port \"cd $path && exec "'\$SHELL'"\" &&exec $SHELL"
  cmd=("$term" "${other_args[@]}" $termexec "--" $SHELL -i -c "ssh -t $userat$host -p $port \"cd $path && exec "'\$SHELL'"\" &&exec $SHELL")
  "${cmd[@]}"
else
    if [ -z "$working_directory" ]; then
        cmd=("$term" "${other_args[@]}" )
        "${cmd[@]}"
        # $term  $other_args
    else
        cmd=("$term" $working_directory_arg="$working_directory" ${other_args[@]} )
        "${cmd[@]}"
        # $term $working_directory_arg="$working_directory" $other_args
    fi
fi
