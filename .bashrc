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
# export PS1="\[\e]0;\u\h:\w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] "

# alias git="~/.bin/git-print"
alias git-submodule_fix_dirty_commit='git submodule deinit -f .; git submodule update --init --remote'
alias git-graph='git log --graph --decorate --oneline $(git rev-list -g --all)'
alias git-prune='du -sh .git && git remote prune origin && git repack && git prune-packed && git reflog expire --all --expire=now && git gc --aggressive --prune=now && du -sh .git'

alias dot="/usr/bin/git --git-dir="$HOME/.dot/.git" --work-tree=$HOME"

#e xport NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# These are ugly, but for reference:
# export TERM=xterm-color
# export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
# export CLICOLOR=1
# export LSCOLORS=ExFxCxDxBxegedabagacad

# alias less='less --RAW-CONTROL-CHARS'
# alias ls="ls -AlFhS"
alias ls='ls --color=auto -AlFhS'
alias mans='man $1 | less +/$2'

source /etc/profile.d/bash_completion.sh

if [[ $TMUX ]]; then
    echo "wat"
    # [NOTE] The default prefix is C-b
    tmux unbind C-b
    tmux set -g prefix C-t
    tmux bind-key C-t send-prefix

    tmux set-option -g mouse on
    tmux setw -g alternate-screen on

    # Address vim mode switching delay (http://superuser.com/a/252717/65504)
    # tmux set-option -s escape-time 0
    tmux set-option -s repeat-time 50

    # https://superuser.com/questions/1560523/how-do-i-resize-tmux-pane-by-holding-down-prefix-and-arrow-key-for-a-while
    # This means you can resize a pane by prefixAlt ← or prefixCtrl ↓ etc.
    # -> These bindings didn't use to exist so beware, you no longer need to set them
    tmux bind-key -r -T prefix M-l resize-pane -L 10
    tmux bind-key -r -T prefix M-r resize-pane -R 10
    tmux bind-key -r -T prefix M-u resize-pane -U 10
    tmux bind-key -r -T prefix M-d resize-pane -D 10
fi

# if [ ! -z ${TRAMP-x} ] ; then
#     return
# fi

# I think this just needs to go in bashrc...
case "$TERM" in
    "dumb")
	PS1="> "
	;;
    xterm*|rxvt*|eterm*|screen*)
        PS1="\[\e]0;\u\h:\w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] "
  	# PS1="${knc}┌─(${kuc}\u${knc}@\h)(\$kituu_info_up1)(\$kituu_info_up2${knc})\$kituu_info_up3${knc}\${kituu_fill}(${kpc}\${kituu_live_pwd}${knc})─┐\n└─(${kituu_smiley}${knc})─> $kituu_user_symbol "
	;;
    linux*)
	PS1="${knc}┌─(${kuc}\u${knc}@\h)(\$kituu_info_up1)(\$kituu_info_up2${knc})\$kituu_info_up3${knc}\${kituu_fill}(${kpc}\${kituu_live_pwd}${knc})─┐\n└─(${kituu_smiley}${knc})─> $kituu_user_symbol "
	;;
    *)
	PS1="> "
	;;
esac




./Users/zooey/Documents/code/site/deploy/scripts/setup.sh
