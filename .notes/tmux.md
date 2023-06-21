
## Rest are tmux learning notes
# https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
# https://apple.stackexchange.com/questions/33736/can-a-terminal-window-be-resized-with-a-terminal-command

# 
# # https://stackoverflow.com/questions/12961656/open-a-program-in-new-terminal-tab-using-shell-script
# # https://github.com/tmux/tmux/wiki/Getting-Started
# 
#   man 1 tmux
#      -2            Force tmux to assume the terminal supports 256 colours.  This is equivalent to -T 256.
# 
#      -C            Start in control mode (see the CONTROL MODE section).  Given twice (-CC) disables echo.
# 
#      -f file       Specify an alternative configuration file.  By default, tmux loads the system configuration file from
#                    /opt/homebrew/etc/tmux.conf, if present, then looks for a user configuration file at ~/.tmux.conf,
#                    $XDG_CONFIG_HOME/tmux/tmux.conf or ~/.config/tmux/tmux.conf.
#      tmux supports a large number of commands which can be used to control its behaviour.  Each command is named and can accept zero or
#      more flags and arguments.  They may be bound to a key with the bind-key command or run from the shell prompt, a shell script, a
#      configuration file or the command prompt.  For example, the same set-option command run from the shell prompt, from ~/.tmux.conf and
#      bound to a key may look like:
# 
#            $ tmux set-option -g status-style bg=cyan
# 
#            set-option -g status-style bg=cyan
# 
#            bind-key C set-option -g status-style bg=cyan
# 
#      Here, the command name is ‘set-option’, ‘-g’ is a flag and ‘status-style’ and ‘bg=cyan’ are arguments.
# 
#            $ tmux kill-window -t :1
# 
#            $ tmux new-window \; split-window -d
# 
#            $ tmux new-session -d 'vi ~/.tmux.conf' \; split-window -d \; attach
# 
# 
#       tmux list-commands

# https://github.com/tmux/tmux/wiki/Getting-Started
# Every window has a name - by default tmux will choose one but it can be changed by the user. Window names do not have to be unique, windows are usually identified by the session and the window index rather than their name.

# A session may be attached to one or more clients, which means it is shown on the outside terminal where that client is running. Any text typed into that outside terminal is sent to the active pane in the current window of the attached session. Sessions do not have an index but they do have a name, which must be unique.

# In summary:
# 
#     Programs run in terminals in panes, which each belong to one window.
#     Each window has a name and one active pane.
#     Windows are linked to one or more sessions.
#     Each session has a list of windows, each with an index.
#     One of the windows in a session is the current window.
#     Sessions are attached to one or more clients, or are detached (attached to no clients).
#     !!! Each client is attached to one session. !!!




# Open a new xterm (e.g. open a new Terminal window)
# Then create a new session, (there are 0 tmux sessions on start by default

# Without arguments, new-session creates a new session and attaches it. Because this is the first session, the tmux server is started and the tmux run from the shell becomes the first client and attaches to it.


# As new windows are opened, the window list grows - if there are too many windows to fit on the width of the terminal, a < or > will be added at the left or right or both to show there are hidden windows.

# The prefix key
# this is just emacs
# C-b ? enters view mode to show text. A pane in view mode has its own key bindings which do not need the prefix key. These broadly follow emacs(1)
# C-b / shows the description of a single key - a prompt at the bottom of the terminal appears. Pressing a key will show its description in the same place. For example, pressing C-b / then ? shows: C-b ? List key bindings

exit

; 

# nodemon src/server.js
# tmux new-window is this required or does it just make one...

# tmux send-keys -t bumblebee 'pg_ctl -l $PGDATA/logfile.log start' Enter



exit


resize-window (resizew) [-aADLRU] [-x width] [-y height] [-t target-window] [adjustment]
show-environment (showenv) [-hgs] [-t target-session] [name]
split-window (splitw) [-bdefhIPvZ] [-c start-directory] [-e environment] [-F format] [-l size] [-t target-pane][shell-command]
list-buffers (lsb) [-F format] [-f filter]
list-clients (lsc) [-F format] [-t target-session]
list-commands (lscm) [-F format] [command]
list-keys (lsk) [-1aN] [-P prefix-string] [-T key-table] [key]
list-panes (lsp) [-as] [-F format] [-f filter] [-t target-window]
list-sessions (ls) [-F format] [-f filter]
list-windows (lsw) [-a] [-F format] [-f filter] [-t target-session]
load-buffer (loadb) [-b buffer-name] [-t target-client] path
lock-client (lockc) [-t target-client]
lock-server (lock) 
lock-session (locks) [-t target-session]
move-pane (movep) [-bdfhv] [-l size] [-s src-pane] [-t dst-pane]
move-window (movew) [-abdkr] [-s src-window] [-t dst-window]
new-session (new) [-AdDEPX] [-c start-directory] [-e environment] [-F format] [-f flags] [-n window-name] [-s session-name] [-t target-session] [-x width] [-y height] [shell-command]
new-window (neww) [-abdkPS] [-c start-directory] [-e environment] [-F format] [-n window-name] [-t target-window] [shell-command]
next-layout (nextl) [-t target-window]
next-window (next) [-a] [-t target-session]
paste-buffer (pasteb) [-dpr] [-s separator] [-b buffer-name] [-t target-pane]
pipe-pane (pipep) [-IOo] [-t target-pane] [shell-command]

