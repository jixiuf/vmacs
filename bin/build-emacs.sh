#!/bin/sh
rm -rf ~/Library/Caches/Homebrew/emacs-plus@28--git
ln -sf ~/repos/emacs ~/Library/Caches/Homebrew/emacs-plus@28--git
brew install --with-native-comp emacs-plus@28