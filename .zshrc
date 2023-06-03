#                  __
#    .-----.-----.|  |--.----.----.
#  __|-- __|__ --||     |   _|  __|
# |__|_____|_____||__|__|__| |____|
# ------------------------------------------------------------
# To reload after saving .zshrc:
# $ source ./zshrc
#
# [todo]
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


# Revmoing / from WORDCHARS to allow meta movement/del with paths

# export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>' # default
# Prevent <M-del> with arrow keys from deleting full path
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

export CLICOLOR=0 # Need to set this for LS to be pretty :^)
export LSCOLORS=gafacadabaegedabagacad
# export lscolors=Exbhcxdxbxegedabagacad

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
alias arp='function _arp(){ arp $@ | column -t };_arp'

# credits https://gist.github.com/natelandau/10654137#file-bash_profile-L87
alias mans='function _mansearch(){ man $1 | grep -iC2 --color=always $2 | less};_mansearch'

# - https://stackoverflow.com/questions/69213355/how-can-i-add-a-flag-to-alias
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




#    _____     ______              __              __
#  _|  |  |_  |   __ \.----.-----.|__|.-----.----.|  |_.-----.
# |_       _| |    __/|   _|  _  ||  ||  -__|  __||   _|__ --|
# |_       _| |___|   |__| |_____||  ||_____|____||____|_____|
#   |__|__|                      |___|
# ------------------------------------------------------------
# [site + db]
export PATH=".dot/bin:$PATH" 
# export PATH="/usr/local/sbin:$PATH"
export DBPATH="/Users/zooey/Documents/code/site/db"
export SITEPATH="/Users/zooey/Documents/code/site/frontend"
# [postgres]
export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"
export PGDATA="/Users/zooey/Documents/code/site/db/postgres/database" # setup default dir for all postgres/pg_ctl commands
export PGPORT="9002"
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
