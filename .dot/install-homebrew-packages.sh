#!/bin/zsh
echo '== BASICS (tools, frameworks) =='

echo 'Installing brew...'
curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh

echo 'Installing npm...'
brew install npm

# [note] Refactor previous python projects to remove 3.10 requirement (axidraw, emojify, etc.)
echo 'Installing python@3.10..'
brew install python@3.10
echo 'Installing python@3.12..'
brew install python@3.12

echo 'Installing imagemagick..'
brew install imagemagick

echo 'Installing fun tools: figlet, cowsay, lolcat..'
brew install figlet
brew install cowsay
brew install lolcat

echo 'Installing ag/the_silver_searcher'..
brew install ag

echo '== CASKS =='
echo 'Installing spotify cask..'
brew install --cask spotify

echo 'Installing vlc cask..'
brew install --cask vlc

echo 'Installing discord cask..'
brew install --cask discord

echo 'Installing firefox cask..'
brew install --cask firefox

echo 'Installing tunnelblick cask..'
brew install --cask tunnelblick

echo 'Installing obs cask..'
brew install --cask obs

echo '[NOTE] Emacs cask not installing - build with native compilation'
# brew install --cask emacs
