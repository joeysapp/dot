#                  __
#    .-----.-----.|  |--.----.----.
#  __|-- __|__ --||     |   _|  __|
# |__|_____|_____||__|__|__| |____|
# ------------------------------------------------------------
# To reload after saving .zshrc:
# $ source ./zshrc
#
# [todo]
# https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html#Process-Substitution
# https://www.gnu.org/software/coreutils/manual/html_node/tee-invocation.html#tee-invocation
# * RC probably shouldn't contain unique stuff, and should be in .zprofile.
#   Although, it is nice to have pretty printing in sshes into the shell.
#   If other things run a shell (like venv) though, they'll see all of this.
# * Lots of helpful startup scripts/aliases to add from here:
# * https://gist.github.com/natelandau/10654137#file-bash_profile-L87

#
# [future-styling-ideas]
# - https://github.com/zsh-users/zsh/tree/master/Functions/Misc
# - https://github.com/zsh-users/zsh/blob/master/Functions/Misc/zstyle%2B
#
#  __     __
# |  |--.|__|.-----.
# |  _  ||  ||     |
# |_____||__||__|__|
# ------------------------------------------------------------
# (todo/read) - https://zsh.sourceforge.io/Doc/Release/Functions.html

# General bash/shell way, very slow for my funny site ps stuff
# (autoloaded seem to execute WAY WAY faster)
# [old]
# export PATH="$HOME/.bin:$PATH"
# chmod u+x script.sh
# ln -s script.sh ~/.bin/script

# [zsh way]
# * https://dev.to/lukeojones/1up-your-zsh-abilities-by-autoloading-your-own-functions-2ngp
# * https://unix.stackexchange.com/a/526429
fpath=(~/.bin $fpath);
autoload -U $fpath[1]/*(.:t)

#    _____     _______         __   __   __
#  _|  |  |_  |     __|.-----.|  |_|  |_|__|.-----.-----.-----.
# |_       _| |__     ||  -__||   _|   _|  ||     |  _  |__ --|
# |_       _| |_______||_____||____|____|__||__|__|___  |_____|
#   |__|__|                                       |_____|
# ------------------------------------------------------------
setopt nobeep;
# setopt no_list_beep;

# Set zsh $SECONDS to be float (Wall clock time, not cpu time)
# (but not date, just $SECONDS. lol.) like via printf "$(date +"%Y-%m-%dT%H:%M:%S")"
# ... or:
# zmodload zsh/datetime -> strftime "%F %T [$epochtime[2]]" $epochtime[1]
typeset -F SECONDS

## - Autocompletion
# https://thevaluable.dev/zsh-completion-guide-examples/
zstyle ':completion:*' menu select 
zstyle ':completion:*' file-list all # shows a list, not just rows
# https://askubuntu.com/a/854365 # autocomplete of user scripts
zstyle ':completion:\*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
# places descriptions above autocomplete
zstyle ':completion:*:*:*:*:descriptions' format '%F{cyan}-- %d --%f'
autoload -Uz compinit && compinit


# Removing '/' from WORDCHARS to allow meta movement/del with paths
#     > this is the default: export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
export WORDCHARS='~!#$%^&*(){}[]<>?.+;-'
MOTION_WORDCHARS='~!#$%^&*(){}[]<>?.+;-/'
# But allow <M-movement> keys to navigate full paths
# https://unix.stackexchange.com/questions/537178/zsh-using-different-wordchars-for-kill-word-and-forward-word-backward-word
''{back,for}ward-word() WORDCHARS=$MOTION_WORDCHARS zle .$WIDGET
zle -N backward-word
zle -N forward-word

## -- History
# https://jdhao.github.io/2021/03/24/zsh_history_setup/
# the detailed meaning of the below three variable can be found in `man zshparam`.
# The meaning of these options can be found in man page of `zshoptions`.
export HISTFILE=~/.zsh_history
export HISTSIZE=1000000   # the number of items for the internal history list
export SAVEHIST=1000000   # maximum number of items for the history file
setopt HIST_SAVE_NO_DUPS  # do not save duplicated command
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS  # remove unnecessary blanks
setopt INC_APPEND_HISTORY # append command to history file immediately after execution
setopt EXTENDED_HISTORY  # record command start time
setopt SHARE_HISTORY
setopt HIST_VERIFY

# -- Visual
# [todo] zsh function loading in fg[], bg[], _bold, _no_bold
autoload -U ~/.bin/colors-extended && colors-extended
# autoload -U colors && colors

# LS colors - you can find info about this with `man ls | less +/LSCOLOR`
export CLICOLOR=0 # Need to set this for LS to be pretty :^)
# export LSCOLORS=gafacadabaegedabagacad # [fg][bg], so first thing dir is cyan fg, black bg.


# a     black
# b     red
# c     green
# d     brown
# e     blue
# f     magenta
# g     cyan
# h     light grey
# A     bold black, usually shows up as dark grey
# B     bold red
# C     bold green
# D     bold brown, usually shows up as yellow
# E     bold blue
# F     bold magenta
# G     bold cyan
# H     bold light grey; looks like bright white
# x     default foreground or background

# 11 items, one char for bgfg, so 22 chars:
# dir, symlink, socket, pipe, exec, block special, char spec, ex-w-setuid-bi-set, ex-w-setgid-bit-set,
# dir-writable-to-others-w-sticky-bit, dir-writable-to-others-wo,sticky-bit
# export LSCOLORS=ga  fa  ca  da  ba  eg  ed  ab  ag  ac  ad
# export LSCOLORS=gx  fx  cx  dx  Bh  Eg  ed  ab  ag  ac  ad
# not sure if spaces are okay?
export LSCOLORS=gxfxcxdxBhEgedabagacad
# making executables bold red, char special bold blue
# export lscolors=Exbhcxdxbxegedabagacad

# https://michurin.github.io/xterm256-color-picker/
# I don't think this works, compiled this into .terminfo/78.. either the : or ; version.
# investigate the 24bit stuff here: https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
# also look into adding dircolors: https://unix.stackexchange.com/questions/91937/mac-os-x-dircolors-not-found
# https://stackoverflow.com/questions/36158093/terminal-color-using-terminfo/36163656#36163656
# ... As a result, the compiled default zsh does not support 24bit color. lol.
# export TERM=xterm-24bit

# If the PROMPT_SUBST option is set,
# the prompt string is first subjected to:
# * parameter expansion
# * command substitution
# * arithmetic expansion
setopt PROMPT_SUBST


PS1="%F{190}%K{000}$(users)@$(hostname):%F{0015}%K{000}%F{039}%K{000}%/%F{015}%K{000}\$ "
# SUPER cool theme and info here:
#        * https://aperiodic.net/phil/prompt/
# more info: # info: https://www.manpagez.com/man/1/zshmisc/
#     all from https://stackoverflow.com/questions/19901044/what-is-k-f-in-oh-my-zsh-theme
#        or https://stackoverflow.com/a/19901630

# old prompts
# Timestamp of [YYYY-MM-DD @ 00:00AM] ttys_id on right side
# RPROMPT="$(tput dim)[%D{%F @ %I:%M%p}] tty%l"


#  __                        __               __
# |  |_.-----.----.--------.|__|.-----.---.-.|  |
# |   _|  -__|   _|        ||  ||     |  _  ||  |
# |____|_____|__| |__|__|__||__||__|__|___._||__|
# ------------------------------------------------------------
alias lr="launchctl-reload $1"
alias arp='function _arp(){ arp $@ | column -t };_arp'

# credits https://gist.github.com/natelandau/10654137#file-bash_profile-L87
# alias mans='function _mansearch(){ man $1 | grep -iC2 --color=always $2 | less};_mansearch'
alias mans='man $1 | less +/$2'

# [chmod octals]
# 000, for rwx. so 001 (1) is --x, 010 (2) is -w-, 110 (6) is rw-, 111 (7) is rwx, in order so
# chmod 755 [file] gives rwx to owner, r-x to group, r-x to others
# chmod 700 [file] gives rwx to owner, --- to group, --- to others.

# - https://stackoverflow.com/questions/69213355/how-can-i-add-a-flag-to-alias
alias ls='LC_COLLATE=C ls -AlFh@'
# A - all files, no . ..
# l - list format
# F - long show / after directories
# h - human readable sizes
# S - sorted largest file at bottom (mebibytes, 2^20)
# si - sorted by (megabytes, 10^6)


#
# -rw-r--r--@  1 zooey  staff    14K Jul 27 20:31 .DS_Store
# drwx------+ 44 zooey  staff   1.4K Jul 27 19:40 .Trash/
# drwxr-xr-x   5 zooey  staff   160B Jul 27 17:13 .config
# 
# @ at the end of the permissions denotes file has extended attributes
# * com.apple-quarantine is added to files that have just been downloaded,
#                        and will then ask the user if they want to allow it to open
# 
# dir/  group
# file  perms    links            
# |owner |        |  [file owner] [
# |perms | others |  user  group  xattr
# |  |   |   |    |   |      |    |
# |  |   |   |    |   |      |    |
# d rwx r-x r-gx   5 zooey  staff  @       160B Jul 27 17:13 .config




#    _____     ______              __              __
#  _|  |  |_  |   __ \.----.-----.|__|.-----.----.|  |_.-----.
# |_       _| |    __/|   _|  _  ||  ||  -__|  __||   _|__ --|
# |_       _| |___|   |__| |_____||  ||_____|____||____|_____|
#   |__|__|                      |___|
# ------------------------------------------------------------
# [dot]
export PATH=".dot/bin:$PATH"
# export PATH="/usr/local/sbin:$PATH"

# [site]
export SITE_FRONTEND_PATH="/Users/zooey/Documents/code/site/frontend"
export SITE_SERVER_PATH="/Users/zooey/Documents/code/site/server"
export JS_COMMON_PATH="/Users/zooey/Documents/code/javascript/common"


# [postgres]
export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"
# setup default dir for all postgres/pg_ctl commands
export PGDATA="$SITE_SERVER_PATH/postgres/database"
export PGPORT="9002"
launchctl setenv LC_ALL "en_us.UTF.8"
# For compilers to find postgresql@15 you may need to set:
# export LDFLAGS="-L/opt/homebrew/opt/postgresql@15/lib"
# export CPPFLAGS="-I/opt/homebrew/opt/postgresql@15/include"






#    _____     _______               __
#  _|  |  |_  |_     _|.-----.-----.|  |.-----.
# |_       _|   |   |  |  _  |  _  ||  ||__ --|
# |_       _|   |___|  |_____|_____||__||_____|
#   |__|__|
# ------------------------------------------------------------
# [ brew ]
eval "$(/opt/homebrew/bin/brew shellenv)"

# [ git ]
alias git="~/.bin/git-print"

# Add this to your .gitprofile for git/git-print to display all remotes/branches
# [alias]
#         branches = branch --all -vvv --color=auto
# * https://stackoverflow.com/questions/3538774/is-it-possible-to-override-git-command-by-git-alias


#    _____     _______         __               __
#  _|  |  |_  |     __|.-----.|  |.---.-.-----.|  |--.
# |_       _| |__     ||  _  ||  ||  _  |__ --||     |
# |_       _| |_______||   __||__||___._|_____||__|__|
#   |__|__|            |__|
# ------------------------------------------------------------

# env | lolcat
# cat ~/.zshrc

# echo '\n === ps '
# ps | lolcat


# du -ckh --si ~/ | sort -h   
# du -hd 1

# find . -maxdepth 1 -type d | sort  


# echo '\n\n === ifconfig | grep inet'
# ifconfig | egrep -o '([[:digit:]]{1,3}\.){3}[[:digit:]]{1,3}' 
# ifconfig | grep inet | lolcat

# echo '\n=== tail -5 ~/.zsh_history ==='
# tail -5 ~/.zsh_history | lolcat --spread=3.5

# echo '\n === df'
# df | lolcat --spread=4
# $a = figlet -k -f chunky "o     helo" | lolcat

echo-bar
(
echo '[shell] <C-r> hist search, <C-l> clear, <C-tab> switch tab'
echo '[emacs] <M-x> outline-show-all to unfold'
echo '[emacs] <C-x r N> insert nums in region'
echo '[emacs] <M-;> to comment region'
echo '[emacs] <C-x-tab> to indent'
echo '[git]   git submodule update --remote to pull upstream branch'
) | lolcat

echo-bar
# Kind of slow tbqh
site-all-status;
echo-bar

# launch_info | lolcat; echo-bar
