#!/bin/bash
# #(foo).*?(bar).*?(!par) 转换成"foo bar !par"
# # echo '-- -d (foo).*?(bar).*?(!par)'|sed 's/\.\*?/ /g'|sed 's/(//g'|sed 's/)//g'|sed 's/--//g'
# cmd="rg -S --no-heading --line-number --color never  "
# pipe=" "

# # remove .*? and () in regexp
# token=`echo "$@"|sed 's/\.\*?/ /g'|sed 's/(//g'|sed 's/)//g'`
# IFS=' ' read -a token<<< "${token}" # split by space to an array

# first="true"
# for element in "${token[@]}"
# do
#     if [[ $element =~ !(.*) ]]; then # start with !
#         if [ "" != "${BASH_REMATCH[1]}" ]; then
#             pipe="$pipe| rg -S --no-heading  --color never -v ${BASH_REMATCH[1]}";
#         fi
#     elif [[ $element =~ -(.*) ]]; then # start with -
#         cmd="$cmd $element"
#     else
#         if [ "$first" =  "true" ]; then
#             cmd="$cmd $element .";
#             first="false"
#         else
#             cmd="$cmd | rg -S --no-heading --color never $element";
#         fi
#     fi
# done
# cmd="$cmd $pipe "

# # echo "$cmd">>/tmp/a.log
# eval $cmd
