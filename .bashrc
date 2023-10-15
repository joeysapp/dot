# 2023-10-15T15:00:00
# note(zooey): Creating bashrc for deployments.
export HISTFILE=~/log/bash-hist.csv
export HISTSIZE=420000000
export HISTFILESIZE=420000001

# https://unix.stackexchange.com/questions/121802/zsh-how-to-check-if-an-option-is-enabled
shopt -s histverify
shopt -s histappend 
shopt -s histreedit

# Default ubuntu prompt
# export PS1="\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$"
# Remove the \h to remove host
export PS1="\[\e]0;\u\h:\w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] "

alias ls="ls -AlFhS"

# alias git="~/.bin/git-print"
alias git-submodule_fix_dirty_commit='git submodule deinit -f .; git submodule update --init --remote'
alias git-graph='git log --graph --decorate --oneline $(git rev-list -g --all)'
alias git-prune='du -sh .git && git remote prune origin && git repack && git prune-packed && git reflog expire --all --expire=now && git gc --aggressive --prune=now && du -sh .git'
