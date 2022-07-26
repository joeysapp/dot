# dot
Managing run configs across computers because `rsync`ing and `mv`ing gets tedious

## usage
Creating a unique git directory and aliasing to `dot` so `git status` doesn't show everything in every subfolder under `/Users/you/*`. 
```
cd $HOME
git init --bare $HOME/.dot.git
# Ignore all files
echo '*' > .dot.git/info/exclude
alias dot="/usr/bin/git --git-dir="$HOME/.dot.git" --work-tree=$HOME"
dot remote add origin https://www.github.com/joeysapp/dot.git
dot fetch
# Sync up to repo, overrides local files
dot reset --hard origin/master
```

## further reading
* [https://www.edwardthomson.com/blog/managing_dotfiles_with_git.htm](https://www.edwardthomson.com/blog/managing_dotfiles_with_git.html)
* [Ask HN: What do you use to manage dotfiles? (https://news.ycombinator.com/item?id=11070797)](https://news.ycombinator.com/item?id=11070797)
