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

# Disable hostname completion
zstyle ':completion:*' hosts off
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle :compinstall filename '/home/anachron/.zshrc'


## PROMPT
autoload -U promptinit && promptinit
autoload -U colors && colors
PROMPT="[%{$fg[magenta]%}%M%{$reset_color%}] \
%{$fg[green]%}%n %{$fg[blue]%}%~%{$fg[yellow]%}$%{$reset_color%} "
BATTERY=1

source /usr/share/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)


## ALIASES
source ~/.aliases


## ENVIRONMENT
export PATH=$HOME/bin/:$HOME/.cabal/bin:$HOME/.gem/ruby/1.9.1/bin/:$PATH
export BROWSER="firefox"
export EDITOR=vim
if [[ -n $(type -p vimpager 2>&-) ]]; then
    export PAGER=vimpager
    alias less='vimpager'
else
    export PAGER=less
fi

## Opts for cabal2arch
export ARCH_HASKELL='Bjørnar Hansen <tilbjornar@gmail.com>'
export PKGBUILD_HASKELL_ENABLE_PROFILING=1

## Opts for owl
export XDG_AUR_HOME="$HOME/.aur"


## KEYBINDS
bindkey -e
bindkey '\e[A'  history-beginning-search-backward # Up
bindkey '\e[B'  history-beginning-search-forward  # Down
bindkey '\e[5~' beginning-of-history              # PageUp
bindkey '\e[6~' end-of-history                    # PageDown
bindkey '\e[2~' quoted-insert                     # Ins
bindkey '\e[3~' delete-char                       # Del
if [[ $TERM == screen-256color ]]; then
    ## tmux
    bindkey '\eOC'  forward-word
    bindkey '\eOD'  backward-word
    bindkey '\e[1~' beginning-of-line   # Home
    bindkey '\e[4~' end-of-line         # End
    bindkey '^H'    backward-kill-word  # C-BS  (delete word backward)
elif [[ $TERM == rxvt-unicode-256color ]]; then
    ## urxvt
    bindkey '\eOc'  forward-word
    bindkey '\eOd'  backward-word
    bindkey '\e[7~' beginning-of-line   # Home
    bindkey '\e[8~' end-of-line         # End
    bindkey '^H'    backward-kill-word  # C-BS  (delete word backward)
    bindkey '^[[3^' kill-word           # C-Del (delete word forward)
elif [[ $TERM == linux ]]; then
    ## linux console
    bindkey '\e[C'  forward-char
    bindkey '\e[D'  backward-char
    bindkey '\e[1~' beginning-of-line # Home
    bindkey '\e[4~' end-of-line       # End
fi

