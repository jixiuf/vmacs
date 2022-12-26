#!/bin/sh
rm -rf ~/Library/Caches/Homebrew/emacs-plus@29--git
ln -sf ~/repos/emacs ~/Library/Caches/Homebrew/emacs-plus@29--git

# cd /usr/local/Homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/ ;git diff >~/.emacs.d/bin/emacs.patch
# cd /usr/local/Homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/
# patch -t -N -p1 <~/.emacs.d/bin/emacs.patch
# cd -

brew reinstall --with-native-comp --with-xwidgets --with-poll emacs-plus@29
# cd /usr/local/Homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/;git stash ;git stash drop
