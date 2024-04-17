#$!/usr/bin/env bash
# https://github.com/cute-jumper/epipe/blob/master/epipe

tty="/dev/$(ps -o tty= -p $$)"

t=$(mktemp *pager-XXXXXX*) || exit 1

default_editor="emacsclient  -t"

[ ! -t 0 ] && cat > $t

[[ "`ps -ef|grep emacs|grep  daemon`"  ]]||emacs --daemon
$default_editor $t <$tty >$tty  && cat $t
# ${EDITOR:-${VISUAL:-$default_editor}} $t <$tty >$tty  && cat $t


# Remove terminal escape sequences (color and move, as well as some funky starship stuff)
# cat - \
#    | sed 's/\x1b\[[0-9;:]*[mGKH]//g' \
#    | sed 's/\x1b\][0-9;:]*[AC]\x1b\\//g' \
#     >> $t
# cat - > "$t"
# emacsclient   -t "$t"
rm -f $t
