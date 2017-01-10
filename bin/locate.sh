#!/bin/bash
# locate foo |grep bar |grep -v Foo ========= ./locate.sh foo bar @Foo
if [ `uname -s` = "Darwin" ] ; then
    # macos
    cmd="mdfind -name $1"
else
    cmd="locate -i $1"

fi

while [ $# -gt 1 ];
do
    if [[ $2 =~ @(.*) ]]; then
        if [ "" != "${BASH_REMATCH[1]}" ]; then
            cmd="$cmd | grep -i -v ${BASH_REMATCH[1]}";
        fi
    else
        cmd="$cmd | grep -i $2";
    fi
    shift;
done

eval $cmd
