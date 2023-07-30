#               __
# .-----.-----.|  |--.----.----.
# |-- __|__ --||     |   _|  __|
# |_____|_____||__|__|__| |____|
# ------------------------------------------------------------
# [term] For info about your terminfo, check man terminfo, then /usr/share/terminfo, or /etc/zshrc or /etc/zprofile
# [todo-zsh-hightlighting] https://gist.github.com/cofirazak/695bc259b0d39a75b1eaaf40bde8d9c4
# [todo-zstyle] https://gist.github.com/netheril96/70be43f8627eea5603f1
# [todo-zstyle] https://github.com/zsh-users/zsh/blob/master/Functions/Misc/zstyle%2B
#
# [?] zshrc probably shouldn't contain unique stuff, and should be in .zprofile
#     (If other things run azsh (like venv) though, they'll get all these settings too)
#
# [todo] Lots of helpful startup scripts/aliases to add from here:
#        https://gist.github.com/natelandau/10654137#file-bash_profile-L87

#
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

# [info] Removing '/' from WORDCHARS to allow meta movement/del with paths
# [ref] https://unix.stackexchange.com/questions/537178/zsh-using-different-wordchars-for-kill-word-and-forward-word-backward-word

# This is the default:
# export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
export WORDCHARS='~!#$%^&*(){}[]<>?.+;-'
MOTION_WORDCHARS='~!#$%^&*(){}[]<>?.+;-/'

# [info] ... but allow <M-movement> keys to navigate full paths
''{back,for}ward-word() WORDCHARS=$MOTION_WORDCHARS zle .$WIDGET
zle -N backward-word
zle -N forward-word

#  __     __         __
# |  |--.|__|.-----.|  |_.-----.----.--.--.
# |     ||  ||__ --||   _|  _  |   _|  |  |
# |__|__||__||_____||____|_____|__| |___  |
#                                   |_____|
# ----------------------------------------------------------------------
# [ref] https://zsh.sourceforge.io/Doc/Release/Options.html#History
# [ref] https://jdhao.github.io/2021/03/24/zsh_history_setup/
# [ref] https://apple.stackexchange.com/questions/427561/macos-zsh-sessions-zsh-history-and-setopt-append-history

# [todo] Look at 'man zshparam' and 'man zshoptions'
# export HISTTIMEFORMAT="[%F %T] " # simple YYYY-M-D time in history -E -10

# https://www.unix.com/man-page/linux/1/fc/
alias h="fc -il"

# https://unix.stackexchange.com/questions/1935/extending-history-search-in-zsh
# uh, where's my history going?
export HISTFILE=~/log/zsh-hist2.csv
export HISTSIZE=51100000 # The number of lines held in memory
export SAVEHIST=51200000  # Maximum number of items for the history file
setopt HIST_FIND_NO_DUPS # Don't show the same command twice in C-s or C-r
setopt INC_APPEND_HISTORY # Write to histfile immediately after execution
setopt EXTENDED_HISTORY  # Write ': <start>:<elapsed>;<command>' (since epoch)
setopt NO_HIST_BEEP

# [todo] https://github.com/ohmyzsh/ohmyzsh/issues/3466
# I don't think this works (for history -E), but 'fc -il 1' works ok.
# export HIST_STAMPS="[%YYYY-%MM-%DDT%hh:%mm:%ss.Z]"
# export HISTTIMEFORMAT="[%YYYY-%MM-%DDT%hh:%mm:%ss.Z]"

 # [todo] Whenever the user enters a line with history expansion, don't execute
#  the line directly; instead, perform history expansion and reload the line into the editing buffer.
setopt HIST_VERIFY

# [todo] see if this is slow
setopt HIST_LEX_WORDS # This allows proper hist multiline commands, but may slowdown search.  

# setopt SHARE_HISTORY # Share history between multiple terms. OR manually do it with fc -RI
# setopt HIST_REDUCE_BLANKS  # Do not write commands starting with a space
# setopt HIST_SAVE_NO_DUPS  # When writing to history, do not write if cmd seen before.


#         __          __
# .-----.|  |_.--.--.|  |.-----.
# |__ --||   _|  |  ||  ||  -__|
# |_____||____|___  ||__||_____|
#             |_____|
# ----------------------------------------------------------------------
# [todo] zsh function loading in fg[], bg[], _bold, _no_bold
autoload -U ~/.bin/colors-extended && colors-extended
# autoload -U colors && colors

# LS colors 
# [info] You can find info about this with `man ls | less +/LSCOLOR`
export CLICOLOR=0 # Need to set this for LS to be pretty :^)
# export LSCOLORS=gafacadabaegedabagacad # [fg][bg], so first thing dir is cyan fg, black bg.

# https://michurin.github.io/xterm256-color-picker/
# .. So weird, there's /usr/share/terminfo/78/xterm-24 but it seems like it isn't actually
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

# [info] 11 items, one char for bgfg, so 22 chars:
# the 11 items are: dir, symlink, socket, pipe, exec, block special, char spec, ex-w-setuid-bi-set, ex-w-setgid-bit-set,
# dir-writable-to-others-w-sticky-bit, dir-writable-to-others-wo,sticky-bit
# 
# export LSCOLORS=ga  fa  ca  da  ba  eg  ed  ab  ag  ac  ad
# export LSCOLORS=gx  fx  cx  dx  Bh  Eg  ed  ab  ag  ac  ad

export LSCOLORS=gxfxcxdxBhEgedabagacad

# making executables bold red, char special bold blue
# export lscolors=Exbhcxdxbxegedabagacad

# [ref] https://www.manpagez.com/man/1/zshmisc/
# [ref] https://stackoverflow.com/questions/19901044/what-is-k-f-in-oh-my-zsh-theme
# [ref] https://stackoverflow.com/a/19901630

# If the PROMPT_SUBST option is set,
# the prompt string is first subjected to:
# * parameter expansion
# * command substitution
# * arithmetic expansion
setopt PROMPT_SUBST
PS1="%F{190}%K{000}$(users)@$(hostname):%F{0015}%K{000}%F{039}%K{000}%/%F{015}%K{000}\$ "

# [todo] SUPER cool theme: https://aperiodic.net/phil/prompt/

# [old prompts]
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
# man-search with paging, press n or N to page.
# alias mans='man $1 | less +/$2' alias does not work
alias mans='function _mansearch(){ man $1 | less +/$2 };_mansearch';

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

alias gitgraph='git log --graph --decorate --oneline $(git rev-list -g --all)'
alias gitprune='du -sh .git && git remote prune origin && git repack && git prune-packed && git reflog expire --all --expire=now && git gc --aggressive --prune=now && du -sh .git'

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
export PATH="/Users/zooey/.bin:$PATH"
export CURL_HOME="/Users/zooey/.config/curl/curl.conf"
# export PATH="/usr/local/sbin:$PATH"


# [site]
export SITE_FRONTEND_PATH="/Users/zooey/Documents/code/site/frontend"
export SITE_SERVER_PATH="/Users/zooey/Documents/code/site/server"
export JS_COMMON_PATH="/Users/zooey/Documents/code/javascript/common"


# [postgres]
export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"

# setup default dir for all postgres/pg_ctl commands
# https://www.postgresql.org/docs/current/libpq-envars.html
export PGDATA="/Users/zooey/Documents/code/database"
export PGPORT="9002"
export PGHOST="127.0.0.1"
# https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNECT-HOSTADDR
export PGHOSTADDR="127.0.0.1" # prevent DNS lookup on startup 
export PGUSER="web"
# export PGPASSFILE="$PGDATA/TODO"
launchctl setenv LC_ALL "en_us.UTF.8"
# For compilers to find postgresql@15 you may need to set:
# export LDFLAGS="-L/opt/homebrew/opt/postgresql@15/lib"
# export CPPFLAGS="-I/opt/homebrew/opt/postgresql@15/include"

# [java]
export JAVA_HOME=$(/usr/libexec/java_home)



#    _____     _______               __
#  _|  |  |_  |_     _|.-----.-----.|  |.-----.
# |_       _|   |   |  |  _  |  _  ||  ||__ --|
# |_       _|   |___|  |_____|_____||__||_____|
#   |__|__|
# ------------------------------------------------------------
# Make sure zsh uses all of the files in ~/.bin, letting us use functions defined in those files.
# https://zsh.sourceforge.io/Doc/Release/Functions.html
# https://unix.stackexchange.com/questions/33255/how-to-define-and-load-your-own-shell-function-in-zsh
# fpath=(
#     ~/.bin
#     $fpath
# )
# autoload -Uz moveto

# https://stackoverflow.com/questions/23524661/how-can-i-calculate-pi-using-bash-command
# pi={ echo -n "scale=100;"; seq 1 2 200 | xargs -n1 -I{} echo '(16*(1/5)^{}/{}-4*(1/239)^{}/{})';} | paste -sd-+ | bc -l
# Not a direct answer to your question about using seq, but pi can be easily computed using bc:
pi=echo "scale=1000; 4*a(1)" | bc -l

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
# echo '[emacs] <M-x> outline-show-all to unfold'
echo '[emacs] <M-!> shell command'
echo '[emacs] <C-x r N> insert nums in region'
echo '[emacs] <M-;> to comment region'
echo '[emacs] <M-e> and <M-a> for f/w sentences'
echo '[emacs] <C-x-tab> to indent'
echo "[bash]  !! $fg[bold]be careful$fg[reset] with commands, easy to accidentally overwrite something !!"
echo "[bash]  launchctl list | sudo tee ~/foo.txt; pipes stdout to your tty and file"
echo '[regex] an example of needing to escape regex operators, all the same result:'

echo '[regex]       .... lol, the -regex here will execute.'
echo '[regex]'
# echo '        find . -regex \./DSC_007\[0-9\]\.NEF'
# echo '        find . -regex \./.\*007\[0-9\]\.NEF      <-- see, gotta escape the *!'
echo '        find -E . -regex "\./.*(07[0-7]).NEF$"'
echo "[emacs] replace-regexp \\(trigger\\)\\([RL]\\) \\,(downcase \\2)2force)"
echo "[emacs] dired W to open file with default application"
) | lolcat

# echo-bar
# launchctl list | grep -v 'com.apple'
echo-bar
# Kind of slow tbqh
site-all-status;
echo-bar

# launch_info | lolcat; echo-bar
