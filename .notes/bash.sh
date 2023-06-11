# -> learn how to use tput save/return carr: 
# echo -e "$(tput sc)Hello world$(tput rc)G'day"
# instead of 
# esc=$'\033'
# echo "${esc}7Hello world${esc}8G'day"

# There's info in bash_regex too

[sed]
Find files *ONLY IN CURRENT DIR*, replace string inside the file to another.

find . \
     -name '*' \
     -not -name '*.bak' \
     -not -path "./.*" \
     -type f \
     -maxdepth 1 \

and if you want to replace all text in them:
     then one of these: 

     -exec sed -i "" -e "s/DBPATH/SITE_SERVER_PATH/" {} \;
     -exec sed -i ".bak" -e "s/DBPATH/SITE_SERVER_PATH/" {} \;
     -exec sed -i "" -e "s/SITEPATH/SITE_FRONTEND_PATH/" {} \;
     -exec sed -i ".bak" -e "s/SITEPATH/SITE_FRONTEND_PATH/" {} \;

  !! no backups :~)
   (or just git init && git add -A . && git commit -m 'backup' prior to running sed in -i mode). 
find . -name '*' -not -name '*.bak' -not -path "./.*" -type f


# sudo find /tmp/ -type f -name *lock -exec ls -l {} \;
#      find:
#     [command] is what you want to execute over results given by the find command.
#     {} is a placeholder that is used to hold results given by the find command.
#     \; says that for each found result, the [command] is executed. You need to escape the ; here and hence \;.

{} + is like this (executing the command once with all results argument):
ls file1.txt file2.txt file3.txt

While it may seem like using {} + is the better option here, it's the opposite. If the find commands throw 50 results, you cannot pass them all as arguments together because there are restrictions on maximum command line length.

{} \; is like this (executing the commands for each found result):
ls file1.txt
ls file2.txt
ls file3.txt




   (THIS GOES RIGHT AFTER IT)
-exec sed -i "" -e "s/SITEPATH/SITE_FRONTEND_PATH/" {} \;

-> According to the sed man page if you run out of disk space on the device you could corrupt a file mid-stride and have a bad output result. If you are working under local source control sed -i "" without backups should be fine most of the time



(or with backups)
find ./ -name '*' -not -name '*.bak' -maxdepth 1 -type f -exec sed -i.bak -e "s/SITEPATH/SITE_FRONTEND_PATH/" {} \;
...
rm *.bak
<OR>



[find]
https://stackoverflow.com/questions/4210042/how-do-i-exclude-a-directory-when-using-find
time find . -name '*' -not -name '*.bak' -not -name ".*" -maxdepth 101
find . -name '*' -not -name '*.bak' -not -name ".*" -maxdepth 101  0.00s user 0.01s system 74% cpu 0.009 total

time find . -name '*' -not -name '*.bak' -not -name ".*" -maxdepth 101

... coyuldn't get this to wokr: hhhahahaX D
find . -name '*.bak' -prune -name '.*' -prune  -print

1.
If -prune doesn't work for you, this will:
find -name "*.js" -not -path "./directory/*"
Caveat: requires traversing all of the unwanted directories.
- One of the comments in the accepted answer points out the problem. -prune does not exclude the directory itself, it exclude its content, which means you are going to get an unwanted line in the output with the excluded directory. 

2.
Use the -prune primary. For example, if you want to exclude ./misc:
find . -path ./misc -prune -o -name '*.txt' -print
To exclude multiple directories, OR them between parentheses.
find . -type d \( -path ./dir1 -o -path ./dir2 -o -path ./dir3 \) -prune -o -name '*.txt' -print
And, to exclude directories with a specific name at any level, use the -name primary instead of -path.
find . -type d -name node_modules -prune -o -name '*.json' -print
- 
Since this is the accepted answer, I feel it should be mentioned here that the reason -print must be added to the end is to prevent the default behavior, which is to also print the names of the pruned directories. @cycollins explains this well in another answer. – 
Thomas
Jan 6, 2022 at 21:16
find . -type d \( -path "./Library/*" -o -path "./.Trash" \)  -prune -o -name '.DS_Store' -print0 | xargs -0 rm From ~/ on my mac cleans it up nicely :) 


[find performance vs bash for loops]
https://unix.stackexchange.com/questions/97084/for-vs-find-in-bash

for loop is probably faster :

time for f in *; do echo "$f"; done
real    0m0.062s
user    0m0.036s
sys     0m0.012s

time find * -prune | while read f; do echo "$f"; done
real    0m0.131s
user    0m0.056s
sys     0m0.060s



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



#
# [bash] https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html#Process-Substitution
# [bash] https://www.gnu.org/software/coreutils/manual/html_node/tee-invocation.html#tee-invocation
