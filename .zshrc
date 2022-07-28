## Path
# export PATH="/usr/local/sbin:$PATH"

# reload
# source ./zshrc

## Aliases

alias dot="git --git-dir="$HOME/.dot.git" --work-tree=$HOME"
alias ls='LC_COLLATE=C ls -AlFh'
# LiSt

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

# https://old.reddit.com/r/linux4noobs/comments/oeqi2j/simple_bashrc_tweak_that_ive_found_extremely/v

# Nice highlighting for tabbing through stuff in term
zstyle ':completion:*' menu select
zstyle ':completion:\*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
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
alias history="fc -l 1"

## NVM Config
export NVM_DIR="$HOME/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# https://zsh.sourceforge.io/Doc/Release/Prompt-Expansion.html#SEC59
# https://www.thegeekstuff.com/2008/09/bash-shell-ps1-10-examples-to-make-your-linux-prompt-like-angelina-jolie/
# PS1="\[$(tput bold)$(tput setb 4)$(tput setaf 7)\]\u@\h:\w $ \[$(tput sgr0)\]"
# function httpdcount {
#   ps aux | grep httpd | grep -v grep | wc -l
# }
# export PS1='\u@\h [`httpdcount`]> '
# 
# for filesize in $(ls -l . | grep "^-" | awk '{print $5}')
# do
#   let totalsize=$totalsize+$filesize
# done
# echo -n "$totalsize"
# export PS1="\u@\h [\$(totalfilesize.sh) bytes]> "
# 
# # https://zsh.sourceforge.io/Intro/intro_14.html#SEC14

## Formatting


## Colors
autoload -U colors && colors
export CLICOLOR=0
export LSCOLORS=gafacadabaegedabagacad
# export lscolors=Exbhcxdxbxegedabagacad

# %U underline
# If the PROMPT_SUBST option is set, the prompt string is first subjected to parameter expansion, command substitution and arithmetic expansion.
setopt PROMPT_SUBST # evals code in ps1/prompt
PS1="%F{190}%K{000}$(users)@$(hostname):%F{0015}%K{000}%F{039}%K{000}%/%F{015}%K{000}\$ "
RPROMPT="$(tput dim)[%D{%F @ %I:%M%p}] tty%l"

# Printouts on tty start

# env | lolcat
# cat ~/.zshrc

# echo '\n === ps '
# ps | lolcat

# echo '\n\n === ifconfig | grep inet'
# ifconfig | egrep -o '([[:digit:]]{1,3}\.){3}[[:digit:]]{1,3}' 
# ifconfig | grep inet | lolcat

echo '\n=== tail -5 ~/.zsh_history ==='
tail -5 ~/.zsh_history | lolcat --spread=3.5

# echo '\n === df'
# df | lolcat --spread=4

figlet what on earth | lolcat
echo '\n\tRemember to C-r for backwards search\n'

# du -ckh --si ~/ | sort -h   
# du -hd 1
# ncdu?


# find . -maxdepth 1 -type d | sort  
