# .dot
Managing files (configs, run commands, fonts, ...) across computers because `rsync`ing and `mv`ing gets tedious

## Install & use
Creating a unique git directory and aliasing to `dot` so `git status` doesn't show everything in every subfolder under `/Users/you/*`. 
```bash
cd $HOME
git init --bare $HOME/.dot/.git

# Ignore all files
echo '*' > .dot/.git/info/exclude

alias dot="/usr/bin/git --git-dir="$HOME/.dot/.git" --work-tree=$HOME"
dot remote add origin https://www.github.com/joeysapp/dot.git

# Sync up to repo, overrides local files
dot fetch
dot reset --hard origin/master
```

## Specifics
### Firefox
- Goto `about:profiles` on Firefox, create a new default profile pointing here.
- userChrome is for the application window itself
  - I want each tab on Firefox to have a custom font
- userContent is custom css styling for websites
  - "I want to always hide a certain div on a certain site"
  - "I want the youtube player to utilize the entire app window 100%"

</details>

## Reading
* [https://www.edwardthomson.com/blog/managing_dotfiles_with_git.htm](https://www.edwardthomson.com/blog/managing_dotfiles_with_git.html)
* [Ask HN: What do you use to manage dotfiles? (https://news.ycombinator.com/item?id=11070797)](https://news.ycombinator.com/item?id=11070797)
