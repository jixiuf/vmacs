cd "$HOME"
cd ~

GREP_OPTIONS="--exclude-dir=\.svn --exclude=*.class --exclude-dir=classes --exclude=*.doc --exclude=*.csv --exclude=TAGS"
export GREP_OPTIONS

LS_COLORS='no=00:fi=00:di=01;37;44:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.cmd=01;32:*.exe=01;32:*.com=01;32:*.btm=01;32:*.bat=01;32:*.sh=01;32:*.csh=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mng=01;35:*.xcf=01;35:*.pcx=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.avi=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.mov=01;35:*.qt=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.pdf=00;32:*.ps=00;32:*.txt=00;32:*.patch=00;32:*.diff=00;32:*.log=00;32:*.tex=00;32:*.doc=00;32:*.mp3=01;32:*.wav=01;32:*.mid=01;32:*.midi=01;32:*.au=01;32:*.ogg=01;32:*.flac=01;32:*.aac=01;32:';
export LS_COLORS 


# export LANG=zh_CN.UTF-8
# export LC_CTYPE="zh_CN.UTF-8"
# export LC_NUMERIC="zh_CN.UTF-8"
# export LC_TIME="zh_CN.UTF-8"
# export LC_COLLATE="zh_CN.UTF-8"
# export LC_MONETARY="zh_CN.UTF-8"
# export LC_MESSAGES="zh_CN.UTF-8"
# export LC_ALL="zh_CN.UTF-8"
alias ls='ls --show-control-chars -F --color=tty'
alias ls="ls --color"
alias la="ls -a --color=auto"
alias ll="ls -lh --color=auto"
alias rm="rm -r"
alias cp="cp -r"
alias ps="ps -ef -W"
alias pp="ps -ef -W |grep"
alias ..="cd .."
alias cd..="cd .."


export LANG=zh_CN.UTF-8
export LC_CTYPE="zh_CN.UTF-8"
export LC_NUMERIC="zh_CN.UTF-8"
export LC_TIME="zh_CN.UTF-8"
export LC_COLLATE="zh_CN.UTF-8"
export LC_MONETARY="zh_CN.UTF-8"
export LC_MESSAGES="zh_CN.UTF-8"
export LC_ALL="zh_CN.UTF-8"

case $TERM in
cygwin)
export PS1='\[\033]0;$MSYSTEM:\w\007
\033[32m\]\u@\h \[\033[33m\w\033[0m\]
$ '
;;
*)
export PS1='\W\$ ' # for emacs and others 
;;
esac

