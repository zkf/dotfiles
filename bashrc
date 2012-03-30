## PATH
# Add private bin dir to PATH
PATH=$HOME/bin/:$HOME/.cabal/bin:$PATH

## Add Android dev tools to PATH
#PATH=$PATH:/opt/android-sdk/platform-tools/:/opt/android-sdk/tools/

## Check for an interactive session
[[ -z "$PS1" ]] && return

## Set bash shell options
shopt -s no_empty_cmd_completion autocd checkwinsize

## If we're logged in through ssh, say so
[[ -n "$SSH_CLIENT" ]] && HOST='[\e[0;35m\H\e[0m] '
PS1="$HOST\[\e[0;32m\]\u \[\e[0;34m\]\W\$(__git_ps1)\[\e[1;32m\]\$\[\e[0m\] "

## Automatically start/attach tmux session when using ssh
if [[ -z "$STARTED_TMUX"  && -n "$SSH_TTY" ]]
then
    STARTED_TMUX=1; export STARTED_TMUX
#    sleep 1
    ( (tmux has-session -t remote && tmux attach-session -t remote) || (tmux new-session -s remote) ) && exit 0
    echo "tmux failed to start"
fi

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

export BROWSER="firefox"

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
## Opts for cabal2arch
export ARCH_HASKELL='Bjørnar Hansen <tilbjornar@gmail.com>'
export PKGBUILD_HASKELL_ENABLE_PROFILING=1

## Opts for owl
export XDG_AUR_HOME="$HOME/projects/aur"
