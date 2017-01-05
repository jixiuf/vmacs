#!/bin/bash
# locate foo |grep bar |grep -v Foo ========= ./everything.sh foo bar @Foo
cmd="locate -i $1"
while [ $# -gt 1 ];
do
    if [[ $2 =~ @(.*) ]]; then
        cmd="$cmd | grep -i -v ${BASH_REMATCH[1]}";
    else
        cmd="$cmd | grep -i $2";
    fi
    shift;
done

eval $cmd
