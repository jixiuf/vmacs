#!/bin/sh
rm -rf ~/Library/Caches/Homebrew/emacs-plus@29--git
ln -sf ~/repos/emacs ~/Library/Caches/Homebrew/emacs-plus@29--git
brew reinstall --with-native-comp --with-xwidgets emacs-plus@29