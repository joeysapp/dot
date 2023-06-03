Read over these basic bash things:


# Easily getting path/fname, and why
https://aty.sdsu.edu/bibliog/latex/debian/bash.html
This use of # to mark the beginning of an edited string, and % for the end, can also be used to simulate the basename and dirname commands in shell scripts:
You can see that the general rule here is: a single  # or % to match the shortest  part; or a double  ## or %% to match the longest  part. 
    
    dirpath=${path%/*}
	base-or-filename=${path##*/}

Notice that the asterisk goes between  the slash and the ##, but after  the %. That's because

	${varname#pattern}
        trims the shortest prefix from the contents of varname that matches the pattern

	${varname##pattern}
        trims the longest prefix from the contents of varname that matches the pattern

	${varname%pattern} 
        trims the shortest suffix from the contents of varname that matches the pattern

	${varname%%pattern}
        trims the longest suffix that matches the pattern from the contents of the shell variabl



$() is creating a subshell in a shell I think
(()) is bash arithmetic.. "command"?


# -> learn how to use tput save/return carr: 
# echo -e "$(tput sc)Hello world$(tput rc)G'day"
# instead of 
# esc=$'\033'
# echo "${esc}7Hello world${esc}8G'day"


So why do people use brackets in if statements? It's because normally you want to decide which branch of the if to run based on some conditional expression (is "$a" equal to "$b", does a certain file exist, etc). [ is actually a command which parses its arguments as a conditional expression (ignoring the final ]), and then exits with either success or failure depending on whether the conditional is true or false. Essentially, [ ] functions as an adapter that lets you use conditional expressions instead of command success/failure in your if statements. In your case, you want success/failure not a conditional expression, so don't use the adapter.

BTW, you'll also sometimes see if [[ some expression ]]; then and if (( some expression )); then. [[ ]] and (( )) are conditional expressions built into bash syntax (unlike [, which is a command). [[ ]] is essentially a better version of [ ] (with some syntax oddities cleaned up and some features added), and (( )) is a somewhat similar construct that does arithmetic expressions.

BTW2 another thing you'll see in scripts is the exit status being tested by checking the special parameter $?, which gives the exit status of the last command. It looks like this:
somecommand
if [ $? -eq 0 ]; then
    echo "Somecommand: success!"
else
    echo "Somecommand: failure!"
fi
I really consider this cargo cult programming. People are used to seeing [ ] conditional expressions in if statements, and this idiom puts the success test in the form of a conditional expression. But let me run through how it works: it takes the exit status of the command, puts it in a conditional expression, has [ ] evaluate that and turn it right back into an exit status so if can use it. That whole rigamarole is unnecessary; just put the command directly in the if statement.

-- source: https://stackoverflow.com/questions/49849957/bash-conditional-based-on-exit-code-of-command

~/Documents/lit/tech/*...xterm-control-sequences.html

https://stackoverflow.com/questions/19482123/extract-part-of-a-string-using-bash-cut-split

https://www.howtogeek.com/812494/bash-string-manipulation/

https://superuser.com/questions/189362/how-to-pipe-command-output-to-other-commands
    https://tldp.org/LDP/abs/html/io-redirection.html
    https://man7.org/linux/man-pages/man1/echo.1p.html
    
https://www.howtogeek.com/562941/how-to-use-the-awk-command-on-linux/



The return code is available in the special parameter $? after the command exits. Typically, you only need to use it when you want to save its value before running another command:

valid_ip "$IP1"
status1=$?
valid_ip "$IP2"
if [ $status1 -eq 0 ] || [ $? -eq 0 ]; then


