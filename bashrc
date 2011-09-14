# Check for an interactive session
[ -z "$PS1" ] && return

shopt -s no_empty_cmd_completion

#PS1='[\u@\h \W]\$ '
#PS1='\[\e[1;32m\][\u@\h \W]\$\[\e[0m\] '
#PS1='\[\e[0;32m\]\u\[\e[m\] \[\e[1;34m\]\w\[\e[m\] \[\e[m\]\[\e[1;32m\]\$ \[\e[m\]\[\e[1;37m\]'
if [ -n "$SSH_CLIENT" ]; then
    HOST='[\e[0;35m\H\e[0m] '
fi
PS1="$HOST\[\e[0;32m\]\u \[\e[0;34m\]\W$(__git_ps1)\[\e[1;32m\]\$\[\e[0m\] "
#PS1='\$ '

## PATH
# Add private bin dir to PATH
PATH=$HOME/bin/:$PATH

# Add Android dev tools to PATH
PATH=$PATH:/opt/android-sdk/platform-tools/:/opt/android-sdk/tools/

## Color for ls
eval $(dircolors -b ~/.dir_colors)

## colors for grep
export GREP_COLOR="1;33"

## Color for man
export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1;44;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;32m'

## Set editor to vim
export EDITOR=vim

## Aliases
# source aliases from file
if [ -f .aliases ]; then
    . .aliases
fi
