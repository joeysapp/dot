# dot
managing run commands/configs across computers
because I keep having to rsync them and this is easier

## usage
Creating a unique git directory and aliasing to `dot` - no accidental commits, `git` doesn't show up in every subfolder. 
```
    $ mkdir .dot.git
    $ git init --bare $HOME/.dot.git
    $ echo '*' > .dot.git/info/exclude
    $ alias dot="/usr/bin/git --git-dir=$HOME/.dot.git" --work-tree=$HOME
    $ dot remote add origin https://www.github.com/joeysapp/dot
    $ dot config status.advice.addIgnoredFile no
    $ dot fetch
    $ dot reset --hard origin/master
    ...
    $ dot status
    $ dot add [file] -f
    $ dot push origin master
```

## further reading
* [https://www.edwardthomson.com/blog/managing_dotfiles_with_git.htm](https://www.edwardthomson.com/blog/managing_dotfiles_with_git.html)
* [Ask HN: What do you use to manage dotfiles? (https://news.ycombinator.com/item?id=11070797)](https://news.ycombinator.com/item?id=11070797)
