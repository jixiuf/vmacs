#!/bin/zsh
# appendPath(newPath)
# 如果 newPath 已经在 PATH 下了， 则不添加
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
# gpgconf --launch gpg-agent
export GPG_TTY="${TTY:-"$(tty)"}"
# gpg-connect-agent updatestartuptty /bye >/dev/null

export APM_OUTPUT_PATH=/tmp/trace/
export LC_ALL=zh_CN.UTF-8
export GO111MODULE=on
appendPath(){
    if [[ -z "${1}" ]]; then
        echo "Usage: appendPath <path>"
        return 1
    fi
    realpath=$1
    if [ -d "$realpath" ]; then
        realpath=`realpath $1`
        if [[ ":${PATH}:" != *":${realpath}:"* ]]; then
            export PATH="${PATH}:${1}"
        fi
    fi
}
prependPath() {
    if [[ -z "${1}" ]]; then
        echo "Usage: prependPath <path>"
        return 1
    fi
    realpath=$1
    if [ -d "$realpath" ]; then
        realpath=`realpath $1`
        if [[ ":${PATH}:" != *":${realpath}:"* ]]; then
            export PATH="${1}:${PATH}"
        fi
    fi
}


prependPath "$HOME/bin"
appendPath "$HOME/.emacs.d/bin"
appendPath "/usr/local/mysql/bin"
appendPath "/usr/local/sbin"
prependPath "/usr/local/bin"
prependPath "/usr/local/emacs/bin"
prependPath "/usr/local/opt/emacs-plus@29/bin"
appendPath "/Library/TeX/texbin"
appendPath "/usr/local/share/dotnet"
appendPath "/Library/Input Methods/Squirrel.app/Contents/MacOS"
appendPath "$HOME/.cargo/bin"
if [ -d $HOME/python/bin/ ]; then
    source $HOME/python/bin/activate
fi
# ;; brew install pyenv-virtualenv
# ;; pyenv install 2.7.13

if [ -d ~/.pyenv/versions/3.9.7/bin/ ]; then
    export PATH="$HOME/.pyenv/versions/3.9.7/bin:$PATH"
fi
export PYTHONPATH="$HOME/.local/lib/python3.9/site-packages"

# # appendPath "$HOME/go_appengine"
# if [ "$HOME/Library/Android/sdk" ]; then
#     export ANDROID_HOME=$HOME/Library/Android/sdk
#     appendPath "$HOME/Library/Android/sdk/platform-tools"
#     appendPath "$HOME/Library/Android/sdk/tools"
# fi
# if [ -d /usr/local/opt/android-sdk ]; then
#     export ANDROID_HOME=/usr/local/opt/android-sdk
#     export ANDROID_SDK_ROOT=/usr/local/opt/android-sdk
#     prependPath "$ANDROID_HOME/bin"
#     prependPath "$ANDROID_HOME/tools"
#     prependPath "$ANDROID_HOME/platform-tools"
# fi
# if [ -d /usr/local/share/android-sdk ]; then
#     export ANDROID_HOME=/usr/local/share/android-sdk
#     export ANDROID_SDK_ROOT=/usr/local/share/android-sdk
#     prependPath "$ANDROID_HOME/bin"
#     prependPath "$ANDROID_HOME/tools"
#     prependPath "$ANDROID_HOME/platform-tools"
# fi

# if [ "$HOME/Library/Android/ndk" ]; then
#     export NDK_ROOT=$HOME/Library/Android/ndk
#     export ANDROID_NDK_ROOT=$HOME/Library/Android/ndk
#     prependPath "$NDK_ROOT/bin"
# fi

# if [ -d /usr/local/opt/android-ndk ]; then
#     export NDK_ROOT=/usr/local/opt/android-ndk
#     export ANDROID_NDK_ROOT=/usr/local/opt/android-ndk
#     prependPath "$NDK_ROOT/bin"
# fi
# if [ -d /usr/local/share/android-ndk ]; then
#     export NDK_ROOT=/usr/local/share/android-ndk
#     export ANDROID_NDK_ROOT=/usr/local/share/android-ndk
#     prependPath "$NDK_ROOT/bin"
# fi



# if [ -d /Library/Java/JavaVirtualMachines/jdk1.7.0_55.jdk/Contents/Home ]; then
#     export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_55.jdk/Contents/Home
# fi

if [ $(uname -s ) = "Darwin" ] ; then
    if [ -d /usr/local/opt/openjdk/ ]; then
        export JAVA_HOME=/usr/local/opt/openjdk/
    else
        export JAVA_HOME=`/usr/libexec/java_home`
    fi
    prependPath "$JAVA_HOME/bin"
    #launchctl setenv PATH $PATH
fi

if [ -x /usr/local/bin/ec ]; then
    export EDITOR=/usr/local/bin/ec
fi

export GOTRACEBACK=crash prog

export NODE_PATH=/usr/local/lib/node_modules

if [ -d /usr/local/go ]; then
    export GOROOT=/usr/local/go
fi
if [ -d /usr/local/opt/go/libexec ]; then
    export GOROOT=/usr/local/opt/go/libexec
fi
# if [ -d ~/go1.17/ ]; then
#     export GOROOT=~/go1.17/
# fi
alias go18='GOROOT=~/go1.18.9/ ~/go1.18.9/bin/go'

prependPath "$GOROOT/bin"
if [ -d $HOME/go ]; then
	export GOPATH=$HOME/go
fi
appendPath "$GOPATH/bin"
appendPath "/usr/local/texlive/2016/bin/x86_64-darwin"

if [ -d /usr/local/include/ ]; then
    export C_INCLUDE_PATH=$C_INCLUDE_PATH:/usr/local/include/
    export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:/usr/local/include/
    export OBJC_INCLUDE_PATH=$OBJC_INCLUDE_PATH:/usr/local/include/
fi
if [ -d /usr/local/lib/ ]; then
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/
fi
# # if [ -d /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.12.sdk/usr/include ]; then
# #     export C_INCLUDE_PATH=$C_INCLUDE_PATH:/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.12.sdk/usr/include
# #     export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.12.sdk/usr/include
# #     export OBJC_INCLUDE_PATH=$OBJC_INCLUDE_PATH:/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.12.sdk/usr/include
# # fi
# if [ -d /usr/local/opt/opencv3 ]; then
#     export OpenCV_DIR=/usr/local/opt/opencv3
#     export C_INCLUDE_PATH=$C_INCLUDE_PATH:$OpenCV_DIR/include
#     export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:$OpenCV_DIR/include
#     export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$OpenCV_DIR/lib
# fi

# # cocos android  begin

# #Add Android SDK & NDK
# if [ -d ~/Documents/tools/adt-bundle-mac-x86_64-20140702/sdk ]; then
#     export ANDROID_SDK_ROOT=~/Documents/tools/adt-bundle-mac-x86_64-20140702/sdk
# else
#     export ANDROID_SDK_ROOT=~/Documents/android/ADT/sdk
# fi
# export ANDROID_NDK_ROOT=~/Documents/android/NDK
# export ANDROID_HOME=~/Documents/android/ADT/sdk
# export NDK_ROOT=~/Documents/android/NDK


# appendPath "$ANDROID_NDK_ROOT"
# appendPath "$PNGQUANT_ROOT"
# appendPath "$ANDROID_SDK_ROOT/build-tools/android-4.4.2"
# appendPath "$ANDROID_SDK_ROOT/tools"
# appendPath "$ANDROID_SDK_ROOT/platform-tools"
# 不知道为什么 GIT_CONFIG_PARAMETERS 有值时 git 命令使用不了
unset GIT_CONFIG_PARAMETERS
