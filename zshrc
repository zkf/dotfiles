# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob nomatch notify interactivecomments
unsetopt beep

## Automatically start/attach tmux session when using ssh
if [[ -z "$STARTED_TMUX"  && -n "$SSH_TTY" ]]; then
    export STARTED_TMUX=1
    ( (tmux has-session -t remote && tmux attach-session -t remote) || (tmux new-session -s remote) ) && exit 0
    echo "tmux failed to start"
fi

## Color for ls
eval $(dircolors -b ~/config/dircolors-solarized.ansi-universal)

## TAB COMPLETION
autoload -U zutil
autoload -Uz compinit
autoload -U complist
autoload -U bashcompinit
compinit

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle :compinstall filename '/home/anachron/.zshrc'


## PROMPT
autoload -U promptinit && promptinit
autoload -U colors && colors
PROMPT="[%{$fg[magenta]%}%M%{$reset_color%}] \
%{$fg[green]%}%n %{$fg[blue]%}%~%{$fg[yellow]%}$%{$reset_color%} "

source /usr/share/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

## PATH
export PATH=$HOME/bin/:$HOME/.cabal/bin:$PATH

## ALIASES
source ~/.aliases

## ENVIRONMENT
export EDITOR=vim
if [[ -n $(type -p vimpager 2>&-) ]]; then
    export PAGER=vimpager
    alias less='vimpager'
else
    export PAGER=less
fi

export BROWSER="firefox"

## Opts for cabal2arch
export ARCH_HASKELL='Bjørnar Hansen <tilbjornar@gmail.com>'
export PKGBUILD_HASKELL_ENABLE_PROFILING=1

## Opts for owl
export XDG_AUR_HOME="$HOME/projects/aur"


## KEYBINDS
bindkey -e
bindkey '\e[A'  history-beginning-search-backward
bindkey '\e[B'  history-beginning-search-forward
bindkey '\e[5~' beginning-of-history # PageUp
bindkey '\e[6~' end-of-history # PageDown
bindkey '\e[2~' quoted-insert # Ins
bindkey '\e[3~' delete-char # Del
bindkey '^H'    backward-kill-word # Del word backward
# bindkey '^[[3~' kill-word # Del word forward
if [[ $TERM == screen-256color ]]; then
    ## tmux
    bindkey '\eOC' forward-word
    bindkey '\eOD' backward-word
    bindkey '\e[1~' beginning-of-line # Home
    bindkey '\e[4~' end-of-line # End
elif [[ $TERM == rxvt-unicode-256color ]]; then
    bindkey '\eOc' forward-word
    bindkey '\eOd' backward-word
    bindkey '\e[7~' beginning-of-line # Home
    bindkey '\e[8~' end-of-line # End
elif [[ $TERM == linux ]]; then
    bindkey '\e[C' forward-word
    bindkey '\e[D' backward-word
    bindkey '\e[1~' beginning-of-line # Home
    bindkey '\e[4~' end-of-line # End
fi

