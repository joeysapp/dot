# 2023-10-15T15:00:00
# note(zooey): Creating bashrc for deployments.
# https://www.redhat.com/sysadmin/history-command
export HISTFILE="/Users/zooey/Documents/code/site/logs/deploy/bash_history"
export HISTFILESIZE=
export HISTSIZE=
HISTTIMEFORMAT="%F %T: "
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
# export HISTCONTROL=ignoredups

# # https://unix.stackexchange.com/questions/121802/zsh-how-to-check-if-an-option-is-enabled
# These are only for zsh AFAIK:
# shopt -s histverify
# shopt -s histappend 
# shopt -s histreedit
# 
# Default ubuntu prompt
# export PS1="\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$"
# Remove the \h to remove host
export PS1="\[\e]0;\u\h:\w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] "

alias ls="ls -AlFhS"

# alias git="~/.bin/git-print"
alias git-submodule_fix_dirty_commit='git submodule deinit -f .; git submodule update --init --remote'
alias git-graph='git log --graph --decorate --oneline $(git rev-list -g --all)'
alias git-prune='du -sh .git && git remote prune origin && git repack && git prune-packed && git reflog expire --all --expire=now && git gc --aggressive --prune=now && du -sh .git'

alias dot="/usr/bin/git --git-dir="$HOME/.dot/.git" --work-tree=$HOME"

 export NVM_DIR="$HOME/.nvm"
 [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
 [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# These are ugly, but for reference:
# export TERM=xterm-color
# export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
# export CLICOLOR=1
# export LSCOLORS=ExFxCxDxBxegedabagacad

# alias less='less --RAW-CONTROL-CHARS'
alias ls='ls --color=auto -AlFhS'
alias mans='man $1 | less +/$2'

# :^)
source /Users/zooey/Documents/code/site/.env.deploy
cd $SITE_DEPLOY_PATH
export PATH="$SITE_DEPLOY_PATH:$PATH"

# [todo] https://www.cyberciti.biz/faq/add-bash-auto-completion-in-ubuntu-linux/
source "$SITE_DEPLOY_PATH/scripts/deploy/bash-autocompletion.sh"
source /etc/profile.d/bash_completion.sh
