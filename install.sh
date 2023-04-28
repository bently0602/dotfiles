#!/bin/sh

# curl -sf -L https://raw.githubusercontent.com/bently0602/dotfiles/main/install.sh | sudo sh

rm -f ~/.tmux.conf
curl -s -o ~/.tmux.conf https://raw.githubusercontent.com/bently0602/dotfiles/main/tmux.conf

rm -f ~/.vimrc
curl -s -o ~/.vimrc https://raw.githubusercontent.com/bently0602/dotfiles/main/vimrc

rm -f ~/.bashrc
curl -s -o ~/.bashrc https://raw.githubusercontent.com/bently0602/dotfiles/main/bashrc

tmux source-file ~/.tmux.conf
source ~/.bashrc
