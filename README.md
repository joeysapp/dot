# dot
managing run commands/configs across computers
because I keep having to rsync them and this is easier

## usage
Placing .git outside of `~/` and aliasing to `dot`, no accidental commits
```
    $ git clone --git-dir=.dot/.git https://www.github.com/joeysapp/dot
    $ echo '*' > .dot/.git/info/exclude
    $ git --git-dir=.dot/.git restore .                    ; overwrite existing local files
    $ alias dot="/usr/bin/git --git-dir=$HOME/.dot/.git"   ; no accidental git use in root
    ...
    $ dot status
    $ dot add [file] -f
    $ dot push origin master
```

## further reading
* [https://www.edwardthomson.com/blog/managing_dotfiles_with_git.htm](https://www.edwardthomson.com/blog/managing_dotfiles_with_git.html)
* [Ask HN: What do you use to manage dotfiles? (https://news.ycombinator.com/item?id=11070797)](https://news.ycombinator.com/item?id=11070797)
