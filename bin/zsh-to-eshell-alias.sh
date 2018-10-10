#!/bin/zsh -i

alias | awk '{print "alias "$0}' | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/^alias ([^= ]+)=(.*)$/alias \1 \2 \$*/g;  s/'\\\''/'/g;" >~/.emacs.d/eshell/alias
