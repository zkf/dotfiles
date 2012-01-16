## PATH
# Add private bin dir to PATH
PATH=$HOME/bin/:$PATH

## Add Android dev tools to PATH
#PATH=$PATH:/opt/android-sdk/platform-tools/:/opt/android-sdk/tools/

## Check for an interactive session
[[ -z "$PS1" ]] && return

## Set bash shell options
shopt -s no_empty_cmd_completion autocd checkwinsize

## If we're logged in thru ssh, say so
[[ -n "$SSH_CLIENT" ]] && HOST='[\e[0;35m\H\e[0m] '
PS1="$HOST\[\e[0;32m\]\u \[\e[0;34m\]\W\$(__git_ps1)\[\e[1;32m\]\$\[\e[0m\] "

## Color for ls
eval $(dircolors -b ~/config/dircolors-solarized.ansi-universal)

## Color for man
export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1;44;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;32m'

## Set up editor and pager
export EDITOR=vim
if [[ $(type -P vimpager 2>&-) ]]; then
    export PAGER=vimpager
    alias less='vimpager'
else
    export PAGER=less
fi

## Source aliases from file
[[ -f ~/config/aliases ]] && . ~/config/aliases

## set the title of urxvt
# not compatible with __git_ps1 in PS1 above 
#case "$TERM" in 
#    rxvt*)
#        set -o functrace
#        trap '[[ "${BASH_SOURCE}" ]] ||
#            printf "\e]0;%s\a" "$BASH_COMMAND" >/dev/tty' DEBUG
#        ;;
#esac
