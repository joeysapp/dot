# dot
managing run commands/configs across computers
because I keep having to rsync them and this is easier

## usage
```
    $ git clone --git-dir=.dotfiles.git https://www.github.com/joeysapp/dot
    $ echo '*' > .dotfiles.git/info/exclude
    $ git --git-dir=.dotfiles.git restore .   ; overwrite existing local files 
    ...
    $ dot status
    $ dot add [file] -f
    $ dot push origin master
```

## further reading
[managing dotfiles with git](https://www.edwardthomson.com/blog/managing_dotfiles_with_git.html)
