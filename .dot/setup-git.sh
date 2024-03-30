touch ~/.gitconfig
echo "[user]
	name = [FOO]
	email = [FOO]
[core]
	excludesfile = /Users/[FOO]/.gitignore
[init]
	defaultBranch = master
[alias]
        # git config --global alias.adog "log --all --decorate --oneline --graph"         
        # https://stackoverflow.com/questions/1057564/pretty-git-branch-graphs
        adog = log --all --decorate --graph --oneline 
        log2 = log --graph --abbrev-commit --decorate --date=relative --all --stat
	branches = branch --all -vvv --color=auto
[credential]
	helper = cache --timeout=100
" >> .gitconfig

touch ~/.gitignore
echo "# emacs buffers
*#*
~/.emacs.d/elpa*
~/.emacs.d/melpa*

# macos
*.DS_STORE

# Node
*node_modules/*

*.zshenv*" >> .gitignore
