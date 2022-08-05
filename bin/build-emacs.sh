#!/bin/sh
rm -rf ~/Library/Caches/Homebrew/emacs-plus@29--git
ln -sf ~/repos/emacs ~/Library/Caches/Homebrew/emacs-plus@29--git

# ./configure "CFLAGS=-DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT"
export CFLAGS="-DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT"
# (shell-command-to-string "ulimit -n")
brew reinstall --with-native-comp --with-xwidgets emacs-plus@29