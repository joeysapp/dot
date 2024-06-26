# 2023-10-15T15:00:00
# https://stackoverflow.com/questions/40952759/how-to-tell-the-difference-between-scp-and-ssh-in-bash-profile
# Return if non-interactive session (scp/rsync)
case $- in
    *i*) ;;
      *) return;;
esac

# note(zooey): Creating bashrc for deployments.
# https://www.redhat.com/sysadmin/history-command
# export HISTFILE="$SITE_DEPLOY_PATH/logs/deploy/bash_history"
export HISTFILE="/joeysapp/var/log/bash.log"
export HISTFILESIZE=512800
export HISTSIZE=512799
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
# alias pm2="/joeysapp/etc/pm2/_pm2"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# These are ugly, but for reference:
# export TERM=xterm-color
# export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
# export CLICOLOR=1
# export LSCOLORS=ExFxCxDxBxegedabagacad

# alias less='less --RAW-CONTROL-CHARS'
alias mans='man $1 | less +/$2'

if [[ $TMUX ]]; then
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
alias _emacs_sudo="sudo /usr/bin/emacs /etc/emacs/site-start.d/00debian.el --file /home/zooey/.emacs.d/init.el"
alias dot="/usr/bin/git --git-dir=$HOME/.dot/.git --work-tree=$HOME"

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

[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;
# Add tab completion for many Bash commands
if which brew &> /dev/null && [ -r "$(brew --prefix)/etc/profile.d/bash_completion.sh" ]; then
	# Ensure existing Homebrew v1 completions continue to work
	export BASH_COMPLETION_COMPAT_DIR="$(brew --prefix)/etc/bash_completion.d";
	source "$(brew --prefix)/etc/profile.d/bash_completion.sh";
elif [ -f /etc/bash_completion ]; then
	source /etc/bash_completion;
fi;

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Add tab completion for `defaults read|write NSGlobalDomain`
# You could just use `-g` instead, but I like being explicit
complete -W "NSGlobalDomain" defaults;

# source ./scripts/desploy/bash-autocompletion-ubuntu.sh
source /etc/profile.d/bash_completion.sh
source /joeysapp/etc/pm2/_autocompletion.sh
source /joeysapp/etc/tmux/_startup.sh

cd /joeysapp
echo "[info] You still have the site/.env.deploy that you likely need."
# cd /Users/zooey/Documents/code/site
# source ./.env.deploy
