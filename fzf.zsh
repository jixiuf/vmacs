# Setup fzf
# ---------
if [[ ! "$PATH" == */usr/local/opt/fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/usr/local/opt/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/usr/local/opt/fzf/shell/completion.zsh" 2> /dev/null


# CTRL-T - Paste the selected file path(s) into the command line
__fsel() {
  local cmd="${FZF_CTRL_T_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | cut -b3-"}"
  setopt localoptions pipefail 2> /dev/null
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" $(__fzfcmd) -m "$@" | while read item; do
    echo -n "${(q)item} "
  done
  local ret=$?
  echo
  return $ret
}

__fzf_use_tmux__() {
  [ -n "$TMUX_PANE" ] && [ "${FZF_TMUX:-0}" != 0 ] && [ ${LINES:-40} -gt 15 ]
}

__fzfcmd() {
  __fzf_use_tmux__ &&
    echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
}

fzf-file-widget() {
  LBUFFER="${LBUFFER}$(__fsel)"
  local ret=$?
  zle reset-prompt
  return $ret
}
zle     -N   fzf-file-widget
# bindkey '^T' fzf-file-widget


# CTRL-R - Paste the selected command from history into the command line
# 改造 fc -rl 1 改成 history -n 1
fzf-history-widget() {
    setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
    item=`history -nr  1 |
      FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=${(qqq)LBUFFER} +m" $(__fzfcmd) `
    LBUFFER="${item} "
    local ret=$?
    zle reset-prompt
    return $ret
}
zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget
if [ $(uname -s ) = "Linux" ] ; then
    fzf-cliphist(){
        item=`cliphist list | fzf | cliphist decode`
        LBUFFER="${item} "
        return items
    }
    zle     -N   fzf-cliphist
    bindkey "^[y" fzf-cliphist
fi




# cdr cd recent directory
ZSH_CDR_DIR=~/.zsh-cdr
mkdir -p $ZSH_CDR_DIR
autoload -Uz chpwd_recent_dirs cdr
autoload -U add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-file $ZSH_CDR_DIR/recent-dirs
zstyle ':chpwd:*' recent-dirs-max 1000
# fall through to cd
zstyle ':chpwd:*' recent-dirs-default yes
# Ensure precmds are run after cd
fzf-redraw-prompt() {
  local precmd
  for precmd in $precmd_functions; do
    $precmd
  done
  zle reset-prompt
}
zle -N fzf-redraw-prompt

# cd 后直接补全
fzf-cdr-widget() {
  # setopt localoptions pipefail 2> /dev/null
  # local cmd="${FZF_ALT_C_COMMAND:-"command find . -type d -depth 1 > /dev/null | cut -b3-"}"
  # local cmd2="${FZF_ALT_C_COMMAND:-" cdr -l |awk '{gsub(/^[0-9]+ +/,\"\")}1' "}"
  local result=`find . -type d -maxdepth 1 2> /dev/null | cut -b3-`
  local result2=`cdr -l|awk '{gsub(/^[0-9]+ +/,"")}1' `

  local dir="$( echo $result2 $result  | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS $FZF_CDR_OPTS " $(__fzfcmd) +m)"
  if [[ -z "$dir" ]]; then
      LBUFFER=""
    zle redisplay
    return 0
  fi
  LBUFFER="cd $dir"
  local ret=$?
  zle fzf-redraw-prompt
  return $ret
}
zle     -N   fzf-cdr-widget

fkill() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
    else
        pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill ${1}
        LBUFFER=""
    fi
    zle fzf-redraw-prompt
}
flsof() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
    else
        pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        LBUFFER="lsof -p $pid"
    fi
    zle fzf-redraw-prompt
}

export FZF_DEFAULT_COMMAND='rg --files'
# https://github.com/junegunn/fzf/wiki/Color-schemes
export FZF_DEFAULT_OPTS="--layout=reverse  --exact --no-height --cycle  --color hl:#ffd900,hl+:#79ed0d,bg+:#616161,info:#616161,prompt:#b4fa72,spinner:107,pointer:#b4fa72  --inline-info --prompt='filter> '  --bind=ctrl-k:kill-line,ctrl-v:page-down,alt-v:page-up,ctrl-m:accept "
# 有些太长，一行显示不下，在最后 3 行进行预览完整命令
export FZF_CTRL_R_OPTS="--preview 'echo {}'  --preview-window up:3:wrap "
export FZF_CDR_OPTS="  "
# export FZF_CTRL_R_OPTS="--sort --exact --preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"

# export FZF_COMPLETION_TRIGGER=''
# bindkey '^T' fzf-completion
# bindkey '^I' $fzf_default_completion
function ssh-fzf () {
  local selected_host=$(grep "Host " ~/.ssh/config |grep -v "^#" | cut -b 6- | fzf --query "$2")
  if [ -n "$selected_host" ]; then
    BUFFER="ssh ${selected_host}"
    zle accept-line
  fi
  # zle reset-prompt
    zle fzf-redraw-prompt
}
zle -N ssh-fzf
