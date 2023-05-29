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
#
# [future-styling-ideas]
# - https://github.com/zsh-users/zsh/tree/master/Functions/Misc
# - https://github.com/zsh-users/zsh/blob/master/Functions/Misc/zstyle%2B
#


#    _____     _______         __   __   __
#  _|  |  |_  |     __|.-----.|  |_|  |_|__|.-----.-----.-----.
# |_       _| |__     ||  -__||   _|   _|  ||     |  _  |__ --|
# |_       _| |_______||_____||____|____|__||__|__|___  |_____|
#   |__|__|                                       |_____|
# ------------------------------------------------------------
## - Autocompletion
# https://thevaluable.dev/zsh-completion-guide-examples/
zstyle ':completion:*' menu select 
zstyle ':completion:*' file-list all # shows a list, not just rows
# https://askubuntu.com/a/854365 # autocomplete of user scripts
zstyle ':completion:\*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
# places descriptions above autocomplete
zstyle ':completion:*:*:*:*:descriptions' format '%F{cyan}-- %d --%f'
autoload -Uz compinit && compinit

## -- History
# https://jdhao.github.io/2021/03/24/zsh_history_setup/
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

# -- Visual
# echo_bar [char] [%_of_zsh]
function echo_bar {
    CHAR_CT=$((COLUMNS * (${2=33.3} / 100.0))); # convert percent to amt of console cols
    # For other stuff:
    # ZSH_WIDTH=$(tput cols); ZSH_HEIGHT=$(tput lines); echo -e "lines\ncols"|tput -S
    CHAR_CT=${CHAR_CT%.*}; # convert to int
    printf "${1:=-}%.0s" {1..$CHAR_CT}; echo '';
};
autoload -U colors && colors
export CLICOLOR=0
export LSCOLORS=gafacadabaegedabagacad
# export lscolors=Exbhcxdxbxegedabagacad
# If the PROMPT_SUBST option is set,
setopt PROMPT_SUBST
# the prompt string is first subjected to:
# * parameter expansion
# * command substitution
# * arithmetic expansion
PS1="%F{190}%K{000}$(users)@$(hostname):%F{0015}%K{000}%F{039}%K{000}%/%F{015}%K{000}\$ "
# Timestamp of [YYYY-MM-DD @ 00:00AM] ttys_id on right side
# RPROMPT="$(tput dim)[%D{%F @ %I:%M%p}] tty%l"

# [info about commands]
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
export PG_COLOR="auto" # Not sure if this does anything on zsh.
# For compilers to find postgresql@15 you may need to set:
# export LDFLAGS="-L/opt/homebrew/opt/postgresql@15/lib"
# export CPPFLAGS="-I/opt/homebrew/opt/postgresql@15/include"
# [dot]
DOT_BASE="git --git-dir=$HOME/.dot/.git --work-tree=$HOME"
function dot {
    if [[ "$1" == "status" && "$@" != *"--help"* && "$@" != *"-h"* ]];
    then
        shift 1; echo_bar - 100;
        eval "$DOT_BASE branches $@"
        echo_bar - 100;
        eval "$DOT_BASE status $@"
    else
        eval "$DOT_BASE $@"
    fi
}


#    _____     _______               __
#  _|  |  |_  |_     _|.-----.-----.|  |.-----.
# |_       _|   |   |  |  _  |  _  ||  ||__ --|
# |_       _|   |___|  |_____|_____||__||_____|
#   |__|__|
# ------------------------------------------------------------
# [ brew ]
eval "$(/opt/homebrew/bin/brew shellenv)"

# [ git ]

# Add this to your .gitprofile:
# [alias]
#         branches = branch --all -vvv --color=auto
# * https://stackoverflow.com/questions/3538774/is-it-possible-to-override-git-command-by-git-alias

# `git status` will now show all remotes and their branches
function git {
    if [[ "$1" == "status" && "$@" != *"--help"* && "$@" != *"-h"* ]];
    then
        shift 1; echo_bar - 100;
        command git branches "$@"
        echo_bar - 100;
        command git status "$@"
    else
        command git "$@"
    fi
}



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

#     args           command and arguments
#     comm           command
#     command        command and arguments
#     cpu            short-term CPU usage factor (for scheduling)
#     etime          elapsed running time
#     flags          the process flags, in hexadecimal (alias f)
#     gid            processes group id (alias group)
#     inblk          total blocks read (alias inblock)
#     jobc           job control count
#     ktrace         tracing flags
#     ktracep        tracing vnode
#     lim            memoryuse limit
#     logname        login name of user who started the session
#     lstart         time started
#     majflt         total page faults
#     minflt         total page reclaims
#     msgrcv         total messages received (reads from pipes/sockets)
#     msgsnd         total messages sent (writes on pipes/sockets)
#     nice           nice value (alias ni)
#     nivcsw         total involuntary context switches
#     nsigs          total signals taken (alias nsignals)
#     nswap          total swaps in/out
#     nvcsw          total voluntary context switches
#     nwchan         wait channel (as an address)
#     oublk          total blocks written (alias oublock)
#     p_ru           resource usage (valid only for zombie)
#     paddr          swap address
#     pagein         pageins (same as majflt)
#     pgid           process group number
#     pid            process ID
#     ppid           parent process ID
#     pri            scheduling priority
#     prsna          persona
#     re             core residency time (in seconds; 127 = infinity)
#     rgid           real group ID
#     rss            resident set size
#     ruid           real user ID
#     ruser          user name (from ruid)
#     sess           session ID
#     sig            pending signals (alias pending)
#     sigmask        blocked signals (alias blocked)
#     sl             sleep time (in seconds; 127 = infinity)
#     start          time started
#     state          symbolic process state (alias stat)
#     svgid          saved gid from a setgid executable
#     svuid          saved UID from a setuid executable
#     tdev           control terminal device number
#     time           accumulated CPU time, user + system (alias cputime)
#     tpgid          control terminal process group ID
#     tsess          control terminal session ID
#     tsiz           text size (in Kbytes)
#     tt             control terminal name (two letter abbreviation)
#     tty            full name of control terminal
#     ucomm          name to be used for accounting
#     uid            effective user ID
#     upr            scheduling priority on return from system call (alias usrpri)
#     user           user name (from UID)
#     utime          user CPU time (alias putime)
#     vsz            virtual size in Kbytes (alias vsize)
#     wchan          wait channel (as a symbolic name)
#     wq             total number of workqueue threads
#     wqb            number of blocked workqueue threads
#     wqr            number of running workqueue threads
#     wql            workqueue limit status (C = constrained thread limit, T = total thread limit)
#     xstat          exit or stop status (valid only for stopped or zombie process)
#
#

function site_status {
    # FRONTEND_STATUS=eval ps -o tty,start,etime,pid,vsz,pcpu,pmem,args | grep '[npm] run start'
    # BACKEND_STATUS=eval ps -o tty,start,etime,pid,vsz,pcpu,pmem,args | grep '[nodemon] nodemon/server.js'

    LABEL="user\t\t pid\t %cpu %me vsize\t     rss   tt\t stat\t start  time\tcommand"
    # todo: printf "${1:=-}%.0s" {1..$CHAR_CT}; echo '';
    # user, pid, %cpu, %me, virtualsize, rss, tt, stat, start, time, command
    FRONTEND=$(
        eval ps auxww |
        grep '[npm] run start' |
        tr -s " " | cut -f $(printf "1,2,3,4,5,6,7,8,9,10,11-") -d " "
    )
    if [[ "$FRONTEND" == *"npm run start"* ]];
    then
        F_START=$(echo $FRONTEND | cut -f 9 -d " ")
        F_UTIME=$(echo $FRONTEND | cut -f 10 -d " ")
        F_CMD=$(echo $FRONTEND | cut -f 11- -d " ")
        echo 'Site/frontend up since: ['$F_START'] with uptime: '$F_UTIME' -> $ '$F_CMD
    else
        echo 'Site/frontend down'
    fi;

    BACKEND=$(
        eval ps auxww |
        grep '[nodemon] nodemon/server.js' |
        tr -s " " | cut -f $(printf "1,2,3,4,5,6,7,8,9,10,11-") -d " "
    )
    # echo $BACKEND

    B_PROC_CT=$(echo $BACKEND | grep '\n' | wc -l);
    if [[ "$BACKEND" == *"nodemon nodemon/server.js"* ]];    
    then
        for i in $(seq 2 $(($B_PROC_CT + 1)))
        do
            B_START=$(echo $BACKEND | cut -f 9 -d " ")
            # Handle two nodemon processes
            B_START=$(echo $B_START | tr -s "\n" ",")
            B_UTIME=$(echo $BACKEND | cut -f 10 -d " " | tr -s "\n" ",")
            B_CMD=$(echo $BACKEND | cut -f 11- -d " " | tr -s "\n" ",")
            echo 'Site/backend['$i'] up since: ['$(echo $B_START | cut -f $i -d ",")'] with uptime: '$(echo $B_UTIME | cut -f $i -d ",") '-> $ '$(echo $B_CMD | cut -f $i -d ",")
        done;
    else
        echo 'Site/backend down'
    fi;
}
function pg_is_up {
    PG_STATUS=$(eval pg_ctl status);
    if [[ "$PG_STATUS" != *"no server running"* ]];
    then
        return 0
    else
        return 1
    fi
}



echo_bar
PGSTATUS=$(eval pg_ctl status)
if pg_is_up;
then
    echo "  << Postgres online >> "${PGSTATUS:26:12}" [proc at "${PGSTATUS:39:-1}"]" | lolcat
else
    echo '  << Postgres offline >>' | lolcat
    echo '  pg_ctl -l $PGDATA/log start'
fi;
echo_bar

site_status

# echo "  cd \$DBPATH; nodemon nodemon/server.js; cd \$SITEPATH; npm run start" | lolcat

echo_bar
echo ' [shell] <C-r> hist search, <C-l> clear, <C-tab> switch tab
 [emacs] <M-x> outline-show-all to unfold, <C-x r N> insert nums in region' | lolcat

echo_bar
~/.dot/bin/list-launch-info.sh | lolcat
echo_bar


