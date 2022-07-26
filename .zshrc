## Users/zooey/.zshrc


# https://old.reddit.com/r/linux4noobs/comments/oeqi2j/simple_bashrc_tweak_that_ive_found_extremely/v
se() {
    search_term=$(echo $@ | sed 's/ /+/g')
    /usr/bin/open -a /Applications/Firefox.app --args -private-window "https://duckduckgo.com/?t=ffsb&q=${search_term}+site%3Astackexchange.com&ia=web"
}


# Nice highlighting for tabbing through stuff in term
zstyle ':completion:*' menu select
# zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
# autoload -Uz compinit && compinit


# Reload without restarting:
# source .zshrc

function gifify {
  if [ -z "$1" ]; then
    echo "$(tput setaf 1)No input file given. Example: gifify example.mov [max width (pixels)]$(tput sgr 0)"
    return 1
  fi

  output_file="${1%.*}.gif"

  echo "$(tput setaf 2)Creating $output_file...$(tput sgr 0)"

  if [ ! -z "$2" ]; then
    maxsize="-vf scale=$2:-1"
  else
    maxsize=""
  fi

  ffmpeg -loglevel panic -i $1 $maxsize -r 10 -vcodec png gifify-tmp-%05d.png
  convert +dither -layers Optimize gifify-tmp-*.png GIF:- | gifsicle --no-warnings --colors 256 --delay=10 --loop --optimize=3 --multifile - > $output_file
  rm gifify-tmp-*.png

  echo "$(tput setaf 2)Done.$(tput sgr 0)"
}




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

## Path
export PATH="/usr/local/sbin:$PATH"

## Aliases
alias ls='LC_COLLATE=C ls -alF'
alias dot="git --git-dir="$HOME/.dot.git" --work-tree=$HOME"

## Colors
autoload -U colors && colors


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
#     \u – Username
#     \h – Hostname
#     \w – Full path of the current working directory
#     \t - hh:mm:s
#     \@ - hh:mm am
#     \! - history number of command
#     $kernel_version: The output of the uname -r command from $kernel_version variable
#     \$?: Status of the last command
# 
#     \e[ – Indicates the beginning of color prompt
#     x;ym – Indicates color code. Use the color code values mentioned below.
#     \e[m – indicates the end of color prompt
# 
# 
#     \a an ASCII bell character (07)
#     \d the date in “Weekday Month Date” format (e.g., “Tue May 26”)
#     \D{format} – the format is passed to strftime(3) and the result is inserted into the prompt string; an empty format results in a locale-specific time representation. The braces are required
#     \e an ASCII escape character (033)
#     \h the hostname up to the first part
#     \H the hostname
#     \j the number of jobs currently managed by the shell
#     \l the basename of the shell’s terminal device name
#     \n newline
#     \r carriage return
#     \s the name of the shell, the basename of $0 (the portion following the final slash)
#     \t the current time in 24-hour HH:MM:SS format
#     \T the current time in 12-hour HH:MM:SS format
#     \@ the current time in 12-hour am/pm format
#     \A the current time in 24-hour HH:MM format
#     \u the username of the current user
#     \v the version of bash (e.g., 2.00)
#     \V the release of bash, version + patch level (e.g., 2.00.0)
#     \w the current working directory, with $HOME abbreviated with a tilde
#     \W the basename of the current working directory, with $HOME abbreviated with a tilde
#     \! the history number of this command
#     \# the command number of this command
#     \$ if the effective UID is 0, a #, otherwise a $
#     \nnn the character corresponding to the octal number nnn
#     \\ a backslash
#     \[ begin a sequence of non-printing characters, which could be used to embed a terminal control sequence into the prompt
#     \] end a sequence of non-printing character
# 



# %U underline
setopt PROMPT_SUBST # evals code in ps1/prompt
# If the PROMPT_SUBST option is set, the prompt string is first subjected to parameter expansion, command substitution and arithmetic expansion.
#PS1="%F{190}%K{000}%m@%h:%F{0015}%K{000}%F{039}%K{000}%/%F{015}%K{000}\$ "
#PS1="%m @ %h : %/ \$ "
#PS1="$(users) @ %m : %/ \$ "
# PS1="%F{190}$(users)%F{0015}@%F{039}$(hostname)%F{0015}:$(pwd)\$ "
PS1="%F{190}%K{000}$(users)@$(hostname):%F{0015}%K{000}%F{039}%K{000}%/%F{015}%K{000}\$ "

# git branch in zsh?
# autoload -Uz vcs_info
# precmd() { vcs_info }
# zstyle ':vcs_info:git:*' formats '(%b)'
# setopt prompt_subst

# strftime format
#RPROMPT="[%D{%y/%m/%f}|%@]"
#RPROMPT="[%D{%j / 366}] [%D{%s}] [%D{%F @ %I:%M%p}]"
# 2022-01-01@12:12
RPROMPT="$(tput dim)[%D{%F @ %I:%M%p}] tty%l"

# RPROMPT="[(%D{%s})]"

# LS colors
export CLICOLOR=1
export LSCOLORS=gafacadabaegedabagacad

env
# cat ~/.zshrc
echo '\n === ps '
ps
echo '\n\n === ifconfig | grep inet'
ifconfig | egrep -o '([[:digit:]]{1,3}\.){3}[[:digit:]]{1,3}' 
#ifconfig | grep inet
echo '\n\n === tail -5 ~/.zsh_history'
tail -5 ~/.zsh_history

echo '\n === df'
df
echo '\n'
echo '\tRemember to C-r for backwards search'
