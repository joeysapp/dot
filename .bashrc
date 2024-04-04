# 2023-10-15T15:00:00
cd /Users/zooey/Documents/code/site
source ./.env.deploy

# note(zooey): Creating bashrc for deployments.
# https://www.redhat.com/sysadmin/history-command
export HISTFILE="$SITE_DEPLOY_PATH/logs/deploy/bash_history"
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

alias mans="man $1 | less +/$2"
alias ls="ls -AlFhS --color"
# /usr/share/emacs/site-lisp/site-start.el 
# https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
alias Semacs="sudo /usr/bin/emacs /etc/emacs/site-start.d/00debian.el --file $HOME/.emacs.d/init.el"
alias dot="/usr/bin/git --git-dir=$HOME/.dot/.git --work-tree=$HOME"

alias tmux="tmux $@"
  	# PS1="${knc}┌─(${kuc}\u${knc}@\h)(\$kituu_info_up1)(\$kituu_info_up2${knc})\$kituu_info_up3${knc}\${kituu_fill}(${kpc}\${kituu_live_pwd}${knc})─┐\n└─(${kituu_smiley}${knc})─> $kituu_user_symbol "

# Handling tramp poorly
case "$TERM" in
    "dumb")
	PS1="> "
	;;
    xterm*|rxvt*|eterm*|screen*)
        PS1="\[\e]0;\u\h:\w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] "
	;;
    linux*)
        PS1="> "
	;;
    *)
	PS1="> "
	;;
esac


if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# source ./scripts/desploy/bash-autocompletion-ubuntu.sh
source ./scripts/deploy/pm2-autocompletion.sh
source ./scripts/deploy/tmux-keybinds.sh
