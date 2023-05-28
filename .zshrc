
#                  __
#    .-----.-----.|  |--.----.----.
#  __|-- __|__ --||     |   _|  __|
# |__|_____|_____||__|__|__| |____|
# ------------------------------------------------------------
# reload after saving .zshrc
# source ./zshrc

# todo(@joeysapp):
#    * read through these:
#      - https://github.com/zsh-users/zsh/tree/master/Functions/Misc
#      - https://github.com/zsh-users/zsh/blob/master/Functions/Misc/zstyle%2B
# 

#     __ __
# .--|  |  |--.
# |  _  |  _  |
# |_____|_____|
# ----------------------------------------
# Add in all our needed terminal commands
export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"

# Default postgres directory for all postgres/pg_ctl commands, etc.
export PGDATA="/Users/zooey/Documents/code/site/db/postgres/database"
export PGPORT="9002"

# Site + DB project
export DBPATH="/Users/zooey/Documents/code/site/db"
export SITEPATH="/Users/zooey/Documents/code/site/frontend"

# Not sure if this does anything?
export PG_COLOR="auto"

# For compilers to find postgresql@15 you may need to set:
# export LDFLAGS="-L/opt/homebrew/opt/postgresql@15/lib"
# export CPPFLAGS="-I/opt/homebrew/opt/postgresql@15/include"

#         __ __
# .-----.|__|  |_
# |  _  ||  |   _|
# |___  ||__|____|
# |_____|
# ------------------------------------------------------------
# todo: Make this work for .dot/annex too
# todo: Add .gitconfig to .dot
# [alias]
#         branches = branch --all -vvv --color=auto

# https://stackoverflow.com/questions/3538774/is-it-possible-to-override-git-command-by-git-alias
function git {
#    if [[ "$1" == "branch" && "$@" != *"--help"* && "$@" != *"-h"* ]];
#    then
#        shift 1
#        command git branches "$@"
    if [[ "$1" == "status" && "$@" != *"--help"* && "$@" != *"-h"* ]];
    then
        shift 1
        command echo ""
        command echo "---------------------------------------------------------"
        command git branches "$@"
        command echo "---------------------------------------------------------"
        command echo ""
        command git status "$@"
    else
        command git "$@"
    fi
}


#  ___ ___
# |   |   |.---.-.----.-----.
# |   |   ||  _  |   _|__ --|
#  \_____/ |___._|__| |_____|
# ----------------------------------------
# export PATH="/usr/local/sbin:$PATH"
export PATH=".dot/bin:$PATH"


#  _______ __ __
# |   _   |  |__|.---.-.-----.
# |       |  |  ||  _  |__ --|
# |___|___|__|__||___._|_____|
# --------------------------------------------------

# - https://stackoverflow.com/questions/69213355/how-can-i-add-a-flag-to-alias

alias dot="git --git-dir="$HOME/.dot/.git" --work-tree=$HOME"

alias ls='LC_COLLATE=C ls -AlFh'
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

# dir/  group
# file  perms    links
# |owner |        |  [file owner]
# |perms | others |  user  group
# |  |   |   |    |   |      |
# |  |   |   |    |   |      |
# d rwx r-x r-x   5 zooey  staff   160B Jul 27 17:13 .config



#  _______ __         __     __ __         __     __   __
# |   |   |__|.-----.|  |--.|  |__|.-----.|  |--.|  |_|__|.-----.-----.
# |       |  ||  _  ||     ||  |  ||  _  ||     ||   _|  ||     |  _  |
# |___|___|__||___  ||__|__||__|__||___  ||__|__||____|__||__|__|___  |
#             |_____|              |_____|                      |_____|
# --------------------------------------------------------------------------------
# https://thevaluable.dev/zsh-completion-guide-examples/
zstyle ':completion:*' menu select 
zstyle ':completion:*' file-list all # shows a list, not just rows
# https://askubuntu.com/a/854365 # autocomplete of user scripts

zstyle ':completion:\*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
# places descriptions above autocomplete
zstyle ':completion:*:*:*:*:descriptions' format '%F{cyan}-- %d --%f'
autoload -Uz compinit && compinit


## https://jdhao.github.io/2021/03/24/zsh_history_setup/
# the detailed meaning of the below three variable can be found in `man zshparam`.
# The meaning of these options can be found in man page of `zshoptions`.
export HISTFILE=~/.zsh_history
export HISTSIZE=1000000   # the number of items for the internal history list
export SAVEHIST=1000000   # maximum number of items for the history file
setopt HIST_IGNORE_ALL_DUPS  # do not put duplicated command into history list
setopt HIST_SAVE_NO_DUPS  # do not save duplicated command
setopt HIST_REDUCE_BLANKS  # remove unnecessary blanks
setopt INC_APPEND_HISTORY_TIME  # append command to history file immediately after execution
setopt EXTENDED_HISTORY  # record command start time
# alias show_last_commands="fc -l 1"


## NVM Config
export NVM_DIR="$HOME/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvmbash_completion



#  ______         __
# |      |.-----.|  |.-----.----.-----.
# |   ---||  _  ||  ||  _  |   _|__ --|
# |______||_____||__||_____|__| |_____|
# ----------------------------------------

# zsh /.zsh_colors
autoload -U colors && colors
export CLICOLOR=0
export LSCOLORS=gafacadabaegedabagacad
# export lscolors=Exbhcxdxbxegedabagacad

# If the PROMPT_SUBST option is set,
# the prompt string is first subjected to parameter expansion, command substitution and arithmetic expansion.
setopt PROMPT_SUBST
PS1="%F{190}%K{000}$(users)@$(hostname):%F{0015}%K{000}%F{039}%K{000}%/%F{015}%K{000}\$ "

# Timestamp of [YYYY-MM-DD @ 00:00AM] ttys_id on right side
# RPROMPT="$(tput dim)[%D{%F @ %I:%M%p}] tty%l"


#  _______                    __
# |     __|.-----.----.--.--.|__|.----.-----.-----.
# |__     ||  -__|   _|  |  ||  ||  __|  -__|__ --|
# |_______||_____|__|  \___/ |__||____|_____|_____|
# ------------------------------------------------------------
# Link $brew with env
eval "$(/opt/homebrew/bin/brew shellenv)"

#  ______        __         __                __
# |   __ \.----.|__|.-----.|  |_.-----.--.--.|  |_.-----.
# |    __/|   _||  ||     ||   _|  _  |  |  ||   _|__ --|
# |___|   |__|  |__||__|__||____|_____|_____||____|_____|
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

DIV="----------------------------------------"
echo $DIV
echo "  pg_ctl -l \$PGDATA'/log' start
  cd \$DBPATH;   nodemon nodemon/server.js
  cd \$SITEPATH; npm run start" | lolcat

echo $DIV
echo ' [shell]      <C-r> for reverse hist search
 [shell]      <C-l> to clear shell
 [*]          <C-tab>, <C-shift-TAB> for tab switch
 [emacs]      <M-x> outline-show-all to unfold md files, or <something>
 [emacs]      <C-x r N>  insert increasing num in region' | lolcat

echo $DIV
~/.dot/bin/list-launch-info.sh | lolcat
echo $DIV

