#!/bin/bash
# xterm.sh - XTerm window control utility
# Reference: http://invisible-island.net/xterm/ctlseqs/ctlseqs.html
# Callable from terminal or Node.js child processes

set -e

# CSI escape sequence
CSI='\e['
OSC='\e]'
ST='\e\\'

# ─────────────────────────────────────────────────────────────────────────────
# Window State Commands
# ─────────────────────────────────────────────────────────────────────────────

cmd_deiconify() { printf "${CSI}1t"; }
cmd_restore() { cmd_deiconify; }

cmd_iconify() { printf "${CSI}2t"; }
cmd_minimize() { cmd_iconify; }

cmd_raise() { printf "${CSI}5t"; }
cmd_front() { cmd_raise; }

cmd_lower() { printf "${CSI}6t"; }
cmd_back() { cmd_lower; }

cmd_refresh() { printf "${CSI}7t"; }

# ─────────────────────────────────────────────────────────────────────────────
# Position & Size Commands
# ─────────────────────────────────────────────────────────────────────────────

cmd_move() {
    local x="${1:-0}" y="${2:-0}"
    # Clamp values
    x=$(( x > 32767 ? 32767 : x < 0 ? 0 : x ))
    y=$(( y > 32767 ? 32767 : y < 0 ? 0 : y ))
    printf "${CSI}3;%d;%dt" "$x" "$y"
}

cmd_resize() {
    local cols="${1:-80}" rows="${2:-24}"
    # Clamp to reasonable values
    cols=$(( cols > 500 ? 500 : cols < 1 ? 1 : cols ))
    rows=$(( rows > 500 ? 500 : rows < 1 ? 1 : rows ))
    printf "${CSI}8;%d;%dt" "$rows" "$cols"
}

cmd_resize_px() {
    local width="${1:-0}" height="${2:-0}"
    # 0 means use display size
    width=$(( width < 0 ? 0 : width ))
    height=$(( height < 0 ? 0 : height ))
    printf "${CSI}4;%d;%dt" "$height" "$width"
}

cmd_lines() {
    local lines="${1:-24}"
    lines=$(( lines > 500 ? 500 : lines < 1 ? 1 : lines ))
    printf "${CSI}%dt" "$lines"
}

# ─────────────────────────────────────────────────────────────────────────────
# Maximize & Fullscreen Commands
# ─────────────────────────────────────────────────────────────────────────────

cmd_maximize() { printf "${CSI}9;1t"; }
cmd_maximize_v() { printf "${CSI}9;2t"; }
cmd_maximize_h() { printf "${CSI}9;3t"; }
cmd_unmaximize() { printf "${CSI}9;0t"; }

cmd_fullscreen() { printf "${CSI}10;1t"; }
cmd_unfullscreen() { printf "${CSI}10;0t"; }
cmd_toggle_fullscreen() { printf "${CSI}10;2t"; }

# ─────────────────────────────────────────────────────────────────────────────
# Query Commands (read terminal response)
# ─────────────────────────────────────────────────────────────────────────────

# Read CSI response from terminal
# Usage: read_response <query_code>
read_response() {
    local query="$1"
    local response=""
    local oldstty

    # Save terminal settings and set raw mode
    oldstty=$(stty -g 2>/dev/null) || true
    stty raw -echo min 0 time 5 2>/dev/null || true

    # Send query
    printf "${CSI}%st" "$query"

    # Read response (format: CSI Ps ; Ps ; Ps t)
    response=$(dd bs=32 count=1 2>/dev/null | tr -d '\000')

    # Restore terminal settings
    stty "$oldstty" 2>/dev/null || true

    # Parse and output the response (strip CSI prefix and t suffix)
    echo "$response" | sed 's/^\x1b\[//; s/t$//'
}

cmd_state() {
    # Returns: 1 = normal, 2 = iconified
    local resp
    resp=$(read_response "11")
    case "$resp" in
        1) echo "normal" ;;
        2) echo "iconified" ;;
        *) echo "$resp" ;;
    esac
}

cmd_pos() {
    # Returns: x;y position
    local resp
    resp=$(read_response "13")
    # Response format: 3;x;y - strip the 3;
    echo "$resp" | sed 's/^3;//'
}

cmd_pos_text() {
    local resp
    resp=$(read_response "13;2")
    echo "$resp" | sed 's/^3;//'
}

cmd_size() {
    # Returns: rows;cols (text area in characters)
    local resp
    resp=$(read_response "18")
    # Response format: 8;rows;cols - strip the 8;
    echo "$resp" | sed 's/^8;//'
}

cmd_size_px() {
    # Returns: height;width (text area in pixels)
    local resp
    resp=$(read_response "14")
    # Response format: 4;height;width - strip the 4;
    echo "$resp" | sed 's/^4;//'
}

cmd_window_size_px() {
    local resp
    resp=$(read_response "14;2")
    echo "$resp" | sed 's/^4;//'
}

cmd_screen() {
    # Returns: rows;cols (screen in characters)
    local resp
    resp=$(read_response "19")
    # Response format: 9;rows;cols - strip the 9;
    echo "$resp" | sed 's/^9;//'
}

cmd_screen_px() {
    # Returns: height;width (screen in pixels)
    local resp
    resp=$(read_response "15")
    # Response format: 5;height;width - strip the 5;
    echo "$resp" | sed 's/^5;//'
}

cmd_cell() {
    # Returns: height;width (cell size in pixels)
    local resp
    resp=$(read_response "16")
    # Response format: 6;height;width - strip the 6;
    echo "$resp" | sed 's/^6;//'
}

cmd_title() {
    local response=""
    local oldstty

    oldstty=$(stty -g 2>/dev/null) || true
    stty raw -echo min 0 time 5 2>/dev/null || true

    printf "${CSI}21t"

    # Response format: OSC l title ST
    response=$(dd bs=256 count=1 2>/dev/null | tr -d '\000')

    stty "$oldstty" 2>/dev/null || true

    # Strip OSC l prefix and ST suffix
    echo "$response" | sed 's/^\x1b\]l//; s/\x1b\\$//'
}

cmd_icon_label() {
    local response=""
    local oldstty

    oldstty=$(stty -g 2>/dev/null) || true
    stty raw -echo min 0 time 5 2>/dev/null || true

    printf "${CSI}20t"

    response=$(dd bs=256 count=1 2>/dev/null | tr -d '\000')

    stty "$oldstty" 2>/dev/null || true

    echo "$response" | sed 's/^\x1b\]L//; s/\x1b\\$//'
}

# ─────────────────────────────────────────────────────────────────────────────
# Title Stack Commands
# ─────────────────────────────────────────────────────────────────────────────

cmd_push_title() { printf "${CSI}22;2t"; }
cmd_pop_title() { printf "${CSI}23;2t"; }

cmd_push_icon() { printf "${CSI}22;1t"; }
cmd_pop_icon() { printf "${CSI}23;1t"; }

cmd_push_both() { printf "${CSI}22;0t"; }
cmd_pop_both() { printf "${CSI}23;0t"; }

cmd_set_title() {
    local title="$1"
    printf "${OSC}2;%s${ST}" "$title"
}

cmd_set_icon() {
    local label="$1"
    printf "${OSC}1;%s${ST}" "$label"
}

cmd_set_both() {
    local title="$1"
    printf "${OSC}0;%s${ST}" "$title"
}

# ─────────────────────────────────────────────────────────────────────────────
# Utility Commands
# ─────────────────────────────────────────────────────────────────────────────

# Open new macOS Terminal with command (preserves existing functionality)
cmd_run() {
    local tmp
    tmp=$(mktemp)
    echo "rm $tmp; cd '$PWD'; $*" > "$tmp"
    chmod 755 "$tmp"
    open -a Terminal "$tmp"
}

# Send raw CSI sequence
cmd_raw() {
    printf "${CSI}%s" "$1"
}

# Send raw XTWINOPS command
cmd_winops() {
    printf "${CSI}%st" "$1"
}

# ─────────────────────────────────────────────────────────────────────────────
# Help
# ─────────────────────────────────────────────────────────────────────────────

cmd_help() {
    cat << 'EOF'
xterm.sh - XTerm window control utility

USAGE: xterm.sh <command> [args...]

WINDOW STATE:
  iconify, minimize     Minimize window
  deiconify, restore    Restore from minimized
  raise, front          Bring window to front
  lower, back           Send window to back
  refresh               Refresh window

POSITION & SIZE:
  move <x> <y>          Move window to pixel position
  resize <cols> <rows>  Resize text area (characters)
  resize-px <w> <h>     Resize window (pixels, 0=display size)
  lines <n>             Resize to n lines (DECSLPP)

MAXIMIZE & FULLSCREEN:
  maximize              Maximize window
  maximize-v            Maximize vertically
  maximize-h            Maximize horizontally
  unmaximize            Restore from maximized
  fullscreen            Enter fullscreen
  unfullscreen          Exit fullscreen
  toggle-fullscreen     Toggle fullscreen

QUERY (read terminal response):
  state                 Window state (normal/iconified)
  pos                   Window position (x;y)
  pos-text              Text area position (x;y)
  size                  Text area size (rows;cols)
  size-px               Text area size in pixels (h;w)
  window-size-px        Window size in pixels (h;w)
  screen                Screen size (rows;cols)
  screen-px             Screen size in pixels (h;w)
  cell                  Cell size in pixels (h;w)
  title                 Window title
  icon-label            Icon label

TITLE STACK:
  push-title            Save title to stack
  pop-title             Restore title from stack
  push-icon             Save icon label to stack
  pop-icon              Restore icon label from stack
  push-both             Save title and icon to stack
  pop-both              Restore title and icon from stack
  set-title <title>     Set window title
  set-icon <label>      Set icon label
  set-both <title>      Set title and icon label

UTILITY:
  run <cmd...>          Open new macOS Terminal with command
  raw <seq>             Send raw CSI sequence
  winops <params>       Send raw XTWINOPS (CSI Ps;...t)
  help                  Show this help

EXAMPLES:
  xterm.sh resize 100 40           # 100 columns, 40 rows
  xterm.sh move 100 200            # Move to x=100, y=200
  xterm.sh maximize
  xterm.sh set-title "My Terminal"
  xterm.sh size                    # Query current size

NODE.JS USAGE:
  const { execSync } = require('child_process');
  execSync('./xterm.sh resize 80 24');
  const size = execSync('./xterm.sh size').toString().trim();
EOF
}

# ─────────────────────────────────────────────────────────────────────────────
# Main dispatcher
# ─────────────────────────────────────────────────────────────────────────────

main() {
    local cmd="${1:-help}"
    shift 2>/dev/null || true

    case "$cmd" in
        # Window state
        iconify|minimize)       cmd_iconify ;;
        deiconify|restore)      cmd_deiconify ;;
        raise|front)            cmd_raise ;;
        lower|back)             cmd_lower ;;
        refresh)                cmd_refresh ;;

        # Position & size
        move)                   cmd_move "$@" ;;
        resize)                 cmd_resize "$@" ;;
        resize-px)              cmd_resize_px "$@" ;;
        lines)                  cmd_lines "$@" ;;

        # Maximize & fullscreen
        maximize)               cmd_maximize ;;
        maximize-v)             cmd_maximize_v ;;
        maximize-h)             cmd_maximize_h ;;
        unmaximize)             cmd_unmaximize ;;
        fullscreen)             cmd_fullscreen ;;
        unfullscreen)           cmd_unfullscreen ;;
        toggle-fullscreen)      cmd_toggle_fullscreen ;;

        # Query
        state)                  cmd_state ;;
        pos)                    cmd_pos ;;
        pos-text)               cmd_pos_text ;;
        size)                   cmd_size ;;
        size-px)                cmd_size_px ;;
        window-size-px)         cmd_window_size_px ;;
        screen)                 cmd_screen ;;
        screen-px)              cmd_screen_px ;;
        cell)                   cmd_cell ;;
        title)                  cmd_title ;;
        icon-label)             cmd_icon_label ;;

        # Title stack
        push-title)             cmd_push_title ;;
        pop-title)              cmd_pop_title ;;
        push-icon)              cmd_push_icon ;;
        pop-icon)               cmd_pop_icon ;;
        push-both)              cmd_push_both ;;
        pop-both)               cmd_pop_both ;;
        set-title)              cmd_set_title "$@" ;;
        set-icon)               cmd_set_icon "$@" ;;
        set-both)               cmd_set_both "$@" ;;

        # Utility
        run)                    cmd_run "$@" ;;
        raw)                    cmd_raw "$@" ;;
        winops)                 cmd_winops "$@" ;;
        help|-h|--help)         cmd_help ;;

        *)
            echo "Unknown command: $cmd" >&2
            echo "Run 'xterm.sh help' for usage" >&2
            exit 1
            ;;
    esac
}

main "$@"
