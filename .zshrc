# @joeysapp
# --------------------------------------------------------------------------------
# [reminder] All of this will be evaluated by anything ran by you w/ zsh (e.g. if you have to use venv for some reason)
fpath=(~/.bin $fpath)
autoload -U $fpath[1]/*(.:t)
autoload -U $HOME/.bin/term-setup-ansi && term-setup-ansi # [OLD] autoload -U colors && colors

setopt nobeep;
setopt no_list_beep;

typeset -F SECONDS
zstyle ':completion:*' menu select 
zstyle ':completion:*' file-list all
zstyle ':completion:\*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:*:*:*:descriptions' format '%F{cyan}-- %d --%f'
autoload -Uz compinit && compinit

''{back,for}ward-word() WORDCHARS=$MOTION_WORDCHARS zle .$WIDGET
zle -N backward-word
zle -N forward-word

# export EDITOR='emacs';

export HISTSIZE=51100000 # The number of lines held in memory
export SAVEHIST=51200000  # Maximum number of items for the history file
setopt HIST_FIND_NO_DUPS # Don't show the same command twice in C-s or C-r
# Omit duplicates and commands that begin with a space from history.
export HISTCONTROL='ignoreboth';
setopt INC_APPEND_HISTORY # Write to histfile immediately after execution
setopt EXTENDED_HISTORY  # Write ': <start>:<elapsed>;<command>' (since epoch)
setopt NO_HIST_BEEP
setopt HIST_VERIFY
setopt HIST_LEX_WORDS 

export CLICOLOR=0
export LSCOLORS=gxfxcxdxbxEgedabagacfd
setopt PROMPT_SUBST

alias lr="launchctl-reload $1"
alias arp='function _arp(){ arp $@ | column -t };_arp'
alias du_here="function _du() { du -ch $1 | sort -h };_du"
alias hist="fc -il"
alias mans='function _mansearch(){ man $1 | less +/$2 };_mansearch';
alias ls='LC_COLLATE=C ls -AlFhS@'
alias git="git-print $@"
alias _emacs_ssh="emacs ssh"
# [TODO] Largely useful on *nix deploys where we need to access /etc/ a lot. IDK where it gets stored w/ NeXT buids.
# alias _emacs_sudo="sudo emacs /etc/emacs/site-start.d/00foobar.el --file $HOME/.emacs.d/init.el"
function pi() { echo "scale=1000; 4*a(1)" | bc -l }

# https://unix.stackexchange.com/questions/71253/what-should-shouldnt-go-in-zshenv-zshrc-zlogin-zprofile-zlogout?newreg=741ab675789d4b1ba96862d40c2bb2d7
# vs like, changing out /etc/zprofile I guess.
source $HOME/.zshalias
source $HOME/.zshpriv

# echo "CHECK .zshpriv, CUZ:"
# echo "HISTFILE=$HISTFILE"
# echo "its even cooler cuz it gets reset after"
# export HISTFILE=$HISTFILE

