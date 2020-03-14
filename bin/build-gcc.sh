#!/bin/bash
patchfile=${PWD}/gcc9.3.patch
# cp $patchfile /usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/
cd /usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/;git checkout ./Formula/gcc.rb; git apply $patchfile
brew install --build-from-source gcc
