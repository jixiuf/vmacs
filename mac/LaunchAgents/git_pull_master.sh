#!/bin/zsh
# cd $@ && git checkout master && git pull
export PATH=$PATH:/usr/local/bin:/usr/local/opt/go/libexec/bin:$HOME/go/bin
cd ~/.emacs.d/&& git pull --commit origin master:master && git push origin master:master
# cd ~/.emacs.d/&& make dump
# cd ~/.emacs.d/&& make pull
# cd ~/.emacs.d/&& make push
# cd ~/.emacs.d/&& make compile
if [ -d ~/Documents/org ]; then
    cd ~/Documents/org && git fetch --all
    cd ~/Documents/org && git pull --commit &&git push
fi

if [ -d ~/repos/dotfiles/ ]; then
    cd ~/repos/dotfiles/  &&git pull --commit &&git push
fi
if [ -d ~/repos/emacs/ ]; then
    cd ~/repos/emacs/  &&git pull
fi
if [ -d ~/repos/magit ]; then
    cd ~/repos/magit && git pull
fi
if [ -d ~/repos/libegit2 ]; then
    cd ~/repos/libegit2 && git pull
fi
if [ -d ~/go/src/golang.org/x/tools ]; then
    cd ~/go/src/golang.org/x/tools/gopls; git pull ;GO111MODULE=on go install
fi
export HOMEBREW_NO_BOTTLE_SOURCE_FALLBACK=1
brew update
HOMEBREW_BOTTLE_DOMAIN=https://mirrors.cloud.tencent.com/homebrew-bottles   brew upgrade
HOMEBREW_BOTTLE_DOMAIN=https://mirrors.aliyun.com/homebrew/homebrew-bottles   brew upgrade
HOMEBREW_BOTTLE_DOMAIN= brew upgrade
find ~/repos -depth 1 -type d  -exec sh -c 'cd {}&&git pull;cd ..'  \;

echo "git_pull_master agent launched at " `date`
