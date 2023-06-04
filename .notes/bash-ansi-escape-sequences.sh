# further reading on ansi escape codes and escape codes, and tput
# https://stackoverflow.com/questions/25879183/can-terminal-app-be-made-to-respect-ansi-escape-codes


# (Which are ESC [ 	0x9B 	CSI 	Control Sequence Introducer 
# But if you want to read about ESC ] 	0x9D 	OSC 	Operating System Command 	, 
# look up stuff like: https://thiagowfx.github.io/2021/12/xterm-enable-ansi-osc-52/

# `ESC [`  is written by:   `\e[`    or     `\033[`
# https://xtermjs.org/docs/api/vtfeatures/

# OSCs:
# ESC ] 1 1 Pt -  	Change colors starting with text background to Pt
# Following will set term background to Pt
# echo -ne "\033]012;rgb:53/19/6f\007"

# ESC ] 1 2 Pt - Change colors starting with text cursor to Pt
# FOllowing will set term cursor to purple:
# echo -ne "\033]012;rgb:53/19/6f\007"

# so now back to CSRS:
#  Pm	A multiple numeric parameter composed of any number of single numeric parameters, separated by ; character(s). Individual values for the parameters are listed with Ps .
#  Pt 	A text parameter composed of printable characters. 

# ... so OSC use Pt, CSI uses Pm, and Pm can contain Ps
# Ps 	A single (usually optional) numeric parameter, composed of one of more digits. 

# So the reason \033]11;rgb:53/19/6f\007 can set the background purple is cause it's taking a Pt, a printable text paramtere
# We need \033[38;5;<<SOMETHING>>m to utilize the xterm 24-bit (or does it allow 32-bit?)

# https://misc.flogisoft.com/bash/tip_colors_and_formatting
# 
# for fg_bg in 38 48 ; do # Foreground / Background
#     for color in {0..255} ; do # Colors
#         # Display the color
#         printf "\e[${fg_bg};5;%sm  %3s  \e[0m" $color $color
#         #
#         # Display 6 colors per lines
#         if [ $((($color + 1) % 6)) == 4 ] ; then
#             echo # New line
#         fi
#     done
#     echo # New line
# done
#  


