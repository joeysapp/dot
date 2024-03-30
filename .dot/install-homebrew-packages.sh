#!/bin/sh
# make sure to chmod +x this
echo 'Installing brew...'
curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh

echo 'Installing npm...'
brew install npm

echo 'Installing python@3.10..'
brew install python@3.10

echo 'Installing fun scripts..'
brew install figlet
brew install cowsay
brew install lolcat
brew install imagemagick


echo 'Installing ag/the_silver_searcher'..
brew install ag

echo 'Installing emacs cask..'
brew install --cask emacs

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
