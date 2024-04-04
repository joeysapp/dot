# @joeysapp
# --------------------------------------------------------------------------------
# [reminder] All of this will be evaluated by anything ran by you w/ zsh (e.g. if you have to use venv for some reason)
fpath=(~/.bin $fpath)
autoload -U $fpath[1]/*(.:t)

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

emacs_cocoa_app="$HOME/Documents/code/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs"
if [ -e $emacs_cocoa_app ]; then
    alias emacs=$emacs_cocoa_app
else
    alias emacs="/usr/bin/emacs"
fi
function pi() { echo "scale=1000; 4*a(1)" | bc -l }

# https://unix.stackexchange.com/questions/71253/what-should-shouldnt-go-in-zshenv-zshrc-zlogin-zprofile-zlogout?newreg=741ab675789d4b1ba96862d40c2bb2d7
# vs like, changing out /etc/zprofile I guess.
source $HOME/.zshpriv
