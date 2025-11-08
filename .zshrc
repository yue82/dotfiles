########################################
# 少し凝った zshrc
# License : MIT
# http://mollifier.mit-license.org/
########################################
# 色を使用出来るようにする
autoload -Uz colors
colors

# emacs 風キーバインドにする
bindkey -e

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# 単語の区切り文字を指定する
autoload -Uz select-word-style
select-word-style default
# ここで指定した文字は単語区切りとみなされる
# / も区切りと扱うので、^W でディレクトリ１つ分を削除できる
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

########################################
# 補完
# 補完機能を有効にする
autoload -Uz compinit
compinit

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..

# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                   /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

########################################
# vcs_info
autoload -Uz vcs_info
autoload -Uz add-zsh-hook

local -A info_formats
info_formats=(
    vcs_source '"%s"'
    base-name  '"%r"'
    branch     '"%b"'
    revision   '"%i"'
    staged     '"%c"'
    unstaged   '"%u"'
    action     '"%a"'
)
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr "*"  # %u で表示する文字列
zstyle ':vcs_info:*' stagedstr "+"    # %c で表示する文字列
zstyle ':vcs_info:*' formats "${(kv)info_formats}"
zstyle ':vcs_info:*' actionformats "${(kv)info_formats}"

function _update_vcs_info_msg() {
    local git_prompt
    LANG=en_US.UTF-8 vcs_info

    local -A GIT_INFO
    GIT_INFO=($(printf "$vcs_info_msg_0_"))
    GIT_INFO=("${(kv@)${(kv@)GIT_INFO#\"}%\"}")
    # revision-short
    [[ -n $GIT_INFO[revision] ]] && GIT_INFO[revision_short]=${(r:7:)GIT_INFO[revision]}

    git_prompt=""
    if [[ -n ${vcs_info_msg_0_} ]]; then
        if [[ -n ${vcs_info_msg_2_} ]]; then
            git_prompt+="%F{red}"
        else
            git_prompt+="%F{green}"
        fi
        git_prompt+="${GIT_INFO[vcs_source]}-[${GIT_INFO[base-name]} ${GIT_INFO[branch]} ${GIT_INFO[revision_short]}"
        if [[ -n ${vcs_info_msg_2_} ]]; then
            git_prompt+="|${GIT_INFO[action]}"
        fi
        git_prompt+="]${GIT_INFO[unstaged]}${GIT_INFO[staged]}%f"
    fi
    RPROMPT="$git_prompt"
}
add-zsh-hook precmd _update_vcs_info_msg

########################################
# オプション
# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# beep を無効にする
setopt no_beep

# フローコントロールを無効にする
setopt no_flow_control

# '#' 以降をコメントとして扱う
setopt interactive_comments

# ディレクトリ名だけでcdする
setopt auto_cd

# cd したら自動的にpushdする
setopt auto_pushd
# 重複したディレクトリを追加しない
setopt pushd_ignore_dups

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups

# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space

# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

# 高機能なワイルドカード展開を使用する
# setopt extended_glob

########################################
# キーバインド

# ^R で履歴検索をするときに * でワイルドカードを使用出来るようにする
# pecoがインストールされているならpeco
function peco-history-selection() {
    BUFFER=`history -n 1 | tac  | awk '!a[$0]++' | peco`
    CURSOR=$#BUFFER
    zle reset-prompt
}
if type peco > /dev/null ; then
    zle -N peco-history-selection
    bindkey '^R' peco-history-selection
else
    bindkey '^R' history-incremental-pattern-search-backward
fi

########################################
# エイリアス

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias mkdir='mkdir -p'

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

########################################
# OS 別の設定
case ${OSTYPE} in
    darwin*)
        #Mac用の設定
        export CLICOLOR=1
        alias ls='ls -G -F'
        ;;
    linux*)
        #Linux用の設定
        alias ls='ls -F --color=auto'
        if uname -r | grep -i 'microsoft' > /dev/null ; then # WSL
            alias xdg-open=wsl-open
            alias open=my-wsl-open
        else
            alias open=xdg-open
        fi
        export LANG=C.UTF-8
        export LC_CTYPE=C.UTF-8
        export LC_ALL=C.UTF-8
        export GOPATH=$HOME/tools/go
        export PATH=$PATH:$GOPATH/bin:/usr/local/go/bin
        ;;
    cygwin*)
        export DISPLAY=:0.0
        export LANG=C.utf8
        alias ls='ls -F --color=auto'
        alias open=cygstart
        export GOPATH='c:\\cygwin\\home\\yue\\go'
        export PATH="$HOME/go/bin:$PATH"
        ;;
esac

########################################
# my settings
########################################
alias la='ls -A'
alias ll='ls -alF'
alias l='ls'

alias cdw='cd ~/trunk/'
alias cdh='cd /mnt/c/Users/yue/'

alias gti='git'

alias bsync_here='browser-sync start --server --files "**/*"'

alias findword='find ./ -type f -print | xargs grep'
alias histgrep='history 0 | grep'
alias psgrep='ps aux | grep'

alias miku='ruby ~/tools/mikutter/mikutter.rb > /dev/null 2>&1 &'
alias gibo='~/tools/gibo/gibo'


function resetmount(){
    killall -9 sshfs;
    fusermount -u $1
}

#プロンプト表示
PROMPT="[%* %n%(?.%{${fg[green]}%}.%{${fg[red]}%})@%{${reset_color}%}%m]%(?.. <%{${fg[red]}%}%?%{${reset_color}%}>) %1~ %(!,#,$) "

# ヒストリーに時刻を記録．-dで時刻付き表示，-fで日付時刻付き表示
setopt extended_history

# 空Enterでpwdとls
function my-enter {
    if [[ -n "$BUFFER" ]]; then
        builtin zle .accept-line
        return 0
    fi
    echo ''
    case $[MY_ENTER_COUNT++] in
        0)
            echo "${fg[cyan]}"$(pwd)"${reset_color}"
            ;;
        1)
            ls
            unset MY_ENTER_COUNT
            ;;
    esac
    zle reset-prompt
}
zle -N my-enter
bindkey '^m' my-enter

export SHELL='zsh'

[[ -e "$HOME/.tmuxinator/tmuxinator.zsh" ]] && source "$HOME/.tmuxinator/tmuxinator.zsh"
[[ -e "$HOME/.env_settings" ]] && source "$HOME/.env_settings"

export PATH="$HOME/my_tools/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

export GTEST_ROOT="$HOME/tools/googletest"
export GTEST_LIBDIR=$GTEST_ROOT"/googletest"
export GTEST_INCLUDE_DIR=$GTEST_ROOT"/googletest/include"

export PATH="/usr/local/bin:/usr/bin/openssl/bin:$PATH"

export PYENV_ROOT="$HOME/.pyenv"
export PYTHONPATH="$PYTHONPATH:/usr/local/lib"
export PATH="$PYENV_ROOT/bin:$PATH"
[[ -s "$HOME/.pyenv" ]] && eval "$(pyenv init -)"

export PATH="$HOME/.rbenv/bin:$PATH"
[[ -s "$HOME/.rbenv" ]] && eval "$(rbenv init -)"

# zmv
autoload -Uz zmv
alias zmv='noglob zmv -W'

my-wsl-open() {
  if [ $# -ne 1 ]; then return 1; fi
  if [ -e "$1" ]; then
    local winpath=$(readlink -f "$1" | xargs -0 wslpath -w)
    powershell.exe start "\"${winpath%?}\""
  else
    powershell.exe start "$1"
  fi
}

# pnpm
export PNPM_HOME="/home/yue/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end


alias emacs='emacs -nw'
export EMACS_SERVER_DIR="$HOME/.emacs.d/server/"
mkdir -p "$EMACS_SERVER_DIR"

if [ -z "$TMUX" ]; then
    export EMACS_SERVER_NAME="server"
else
    export EMACS_SERVER_NAME="window-$(tmux display-message -p '#{window_index}')"
fi
export EMACS_SERVER_SOCKET="${EMACS_SERVER_DIR}${EMACS_SERVER_NAME}"

emacs-server-show() {
    if emacsclient -s "${EMACS_SERVER_SOCKET}" -n -e "(message \"\n\")" > /dev/null 2>&1; then
        echo "server: ${EMACS_SERVER_NAME}"
        return 0
    else
        echo "No emacs server."
        return 1
    fi
}

emacs-server-with-tmux() {
    if emacsclient -s "${EMACS_SERVER_SOCKET}" -e "(message \"\n\")" > /dev/null 2>&1; then
        return 0
    else
        command emacs --daemon="$EMACS_SERVER_NAME" > /dev/null 2>&1 &
        local i=0
        local MAX_TRIES=10
        while [ $i -lt $MAX_TRIES ]; do
            if emacsclient -s "${EMACS_SERVER_SOCKET}" -n -e "(message \"\n\")" > /dev/null 2>&1; then
                return 0
            fi
            sleep 1
            i=$((i + 1))
        done
        return 1
    fi
}

emacs-client-with-tmux() {
    if [ -z "$TMUX" ]; then
        emacsclient -t -a emacs "$@"
        return $?
    fi
    EMACS_SERVER_NAME="window-$(tmux display-message -p '#{window_index}')"
    emacs-server-with-tmux
    emacsclient -s "${EMACS_SERVER_SOCKET}" -t -a emacs "$@"
}

ekill() {
    if [ -z "$TMUX" ]; then
        EMACS_SERVER_NAME="server"
    else
        EMACS_SERVER_NAME="window-$(tmux display-message -p '#{window_index}')"
    fi
    if emacsclient -s "${EMACS_SERVER_SOCKET}" -n -e '(kill-emacs)' > /dev/null 2>&1; then
        sleep 0.2
        if emacsclient -s "${EMACS_SERVER_SOCKET}" -n -e "(message \"\n\")" > /dev/null 2>&1; then
            return 1
        else
            rm -f "${EMACS_SERVER_DIR}${EMACS_SERVER_NAME}"*
            return 0
        fi
    else
        rm -f "${EMACS_SERVER_DIR}${EMACS_SERVER_NAME}"*
        return 0
    fi
}

alias e='emacs-client-with-tmux'
export EDITOR="zsh -i -c 'emacs-client-with-tmux'"
export VISUAL="$EDITOR"
export TERM=xterm-256color

[[ -s ~/.env_settings ]] && source ~/.env_settings
