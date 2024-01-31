# export currentDir_PS1="%F{231}$(users)@${HOST/\.*/} %F{114}${PWD/*\//}/%F{015} "

# Set prompt colors based on machine
# [REF] https://robotmoon.com/zsh-prompt-generator/
# _HOSTNAME="$(hostname)"

# Removing .local from the end of hostanem
_HOSTNAME=${HOST/\.*/}
if [[ "$_HOSTNAME" == "kittenblob" ]]; then
    # The HOST regexp is removing trailering ".local", lol.
    export PS1="%F{231}$(users)@${HOST/\.*/} %F{114}%/%F{015} "
elif [[ "$_HOSTNAME" == "foxy" || "$_HOSTNAME" == "no_idea_1" || "$_HOSTNAME" == "zooeys-MacBook-Pro" ]]; then
    export PS1="%F{231}$(users)@$(echo "foxy"):%F{214}%/%F{015} "
else
    export PS1="%F{231}$(users)@${HOST/\.*/} %F{114}%/%F{015} "
fi

# Set zsh history file based on machine ( goes to ~/log/zsh-hist-[host]
source /Users/zooey/.bin/zsh-set-hist-file

eval "$(/opt/homebrew/bin/brew shellenv)"
export HOMEBREW_NO_ANALYTICS=1

# [info] Prevent the .zsh_Apple_Terminal_Use from creating .zsh_sessions,
export SHELL_SESSIONS_DISABLE=1

# Not sure if arduino sees zshenv or zshrc..
# export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/opt/homebrew/opt/postgresql@15/bin:/Users/zooey/.bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/Apple/usr/bin:/Applications/Wireshark.app/Contents/MacOS:/Users/zooey/.bin/python/bin"
# export PATH="/Users/zooey/.bin/python-arduino/bin:$PATH"

# [site/local] Mostly used for the common javascript library
export SITE_PATH=/Users/zooey/Documents/code/site
