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
            alias open=wsl-open
        else
            alias open=xdg-open
        fi
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

alias emacs='TERM=xterm-256color emacs -nw'
alias e='TERM=xterm-256color emacsclient -c -a "" -nw'
alias ekill="emacsclient -e '(kill-emacs)'"

if ! emacsclient -e 0 > /dev/null 2>&1; then
    cd > /dev/null 2>&1
    emacs --daemon
    cd - > /dev/null 2>&1
fi

alias cdw='cd ~/trunk/'

alias gti='git'

alias py27='. ~/venv/py27/bin/activate'
alias py34='. ~/venv/py34/bin/activate'

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
function my_enter {
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
zle -N my_enter
bindkey '^m' my_enter

export EDITOR='emacsclient --alternate-editor emacs'
export SHELL='zsh'
export DATA_DIR=$HOME'/data'
# export DATA_DIR='/mnt/hdd/data'

[[ -e "$HOME/.tmuxinator/tmuxinator.zsh" ]] && source "$HOME/.tmuxinator/tmuxinator.zsh"
[[ -e "$HOME/.env_settings" ]] && source "$HOME/.env_settings"

export PATH="$HOME/my_tools/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

export GTEST_ROOT="$HOME/tools/googletest"
export GTEST_LIBDIR=$GTEST_ROOT"/googletest"
export GTEST_INCLUDE_DIR=$GTEST_ROOT"/googletest/include"

export PATH="/usr/local/bin:$PATH"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
[[ -s "$HOME/.pyenv" ]] && eval "$(pyenv init -)"

export PATH="$HOME/.rbenv/bin:$PATH"
[[ -s "$HOME/.rbenv" ]] && eval "$(rbenv init -)"

# zmv
autoload -Uz zmv
alias zmv='noglob zmv -W'
