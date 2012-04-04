# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob nomatch notify
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall

## Automatically start/attach tmux session when using ssh
if [[ -z "$STARTED_TMUX"  && -n "$SSH_TTY" ]]; then
    STARTED_TMUX=1; export STARTED_TMUX
#    sleep 1
    ( (tmux has-session -t remote && tmux attach-session -t remote) || (tmux new-session -s remote) ) && exit 0
    echo "tmux failed to start"
fi

## Color for ls
eval $(dircolors -b ~/config/dircolors-solarized.ansi-universal)

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle :compinstall filename '/home/anachron/.zshrc'

autoload -Uz compinit && compinit
# End of lines added by compinstall

autoload -U promptinit && promptinit
autoload -U colors && colors
PROMPT="[%{$fg[magenta]%}%M%{$reset_color%}] \
%{$fg[green]%}%n %{$fg[blue]%}%~%{$fg[yellow]%}$%{$reset_color%} "

## PATH
PATH=$HOME/bin/:$HOME/.cabal/bin:$PATH

## ALIASES
source ~/.aliases

## Set up editor and pager
EDITOR=vim
if [[ -n $(type -p vimpager 2>&-) ]]; then
    PAGER=vimpager
    alias less='vimpager'
else
    PAGER=less
fi

BROWSER="firefox"

## Opts for cabal2arch
ARCH_HASKELL='Bjørnar Hansen <tilbjornar@gmail.com>'
PKGBUILD_HASKELL_ENABLE_PROFILING=1

## Opts for owl
XDG_AUR_HOME="$HOME/projects/aur"


## KEYBINDS
bindkey '\e[A'  history-beginning-search-backward
bindkey '\e[B'  history-beginning-search-forward
bindkey '\e[1~' beginning-of-line # Home
bindkey '\e[4~' end-of-line # End
bindkey '\e[5~' beginning-of-history # PageUp
bindkey '\e[6~' end-of-history # PageDown
bindkey '\e[2~' quoted-insert # Ins
bindkey '\e[3~' delete-char # Del
bindkey '^H'    backward-kill-word # Del word backward
# bindkey '^[[3~' kill-word # Del word forward
if [[ $TERM == screen-256color ]]; then
    bindkey '\eOC' forward-word
    bindkey '\eOD' backward-word
elif [[ $TERM == rxvt-unicode-256color ]]; then
    bindkey '\eOc' forward-word
    bindkey '\eOd' backward-word
fi