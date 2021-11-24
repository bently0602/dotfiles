#!/bin/sh

# curl -sf -L https://raw.githubusercontent.com/bently0602/dotfiles/main/install.sh | sudo sh

rm -f ~/.tmux.conf
curl -o ~/.tmux.conf https://raw.githubusercontent.com/bently0602/dotfiles/main/tmux.conf

rm -f ~/.vimrc
curl -o ~/.vimrc https://raw.githubusercontent.com/bently0602/dotfiles/main/.vimrc
