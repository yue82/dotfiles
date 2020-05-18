#!/bin/bash

locate=$1
if [ -z $locate ]; then
    locate='default'
fi

# env setting
ENV_SETTING='.env_settings'
if [ ! -e $HOME/dotfiles/$locate$ENV_SETTING ]; then
    touch $HOME/dotfiles/$locate$ENV_SETTING
fi
ln -s $HOME/dotfiles/$locate$ENV_SETTING $HOME/$ENV_SETTING
echo 'ln '$HOME/$ENV_SETTING" ($locate)"


# link dot files/dirs
DOT_FILES=(.zshrc .bashrc .emacs .gitconfig .screenrc .tmux.conf)
for file in ${DOT_FILES[@]}
do
    ln -s $HOME/dotfiles/$file $HOME/$file
    echo 'ln '$HOME/$file
done

DOT_DIRS=(.emacs.d .tmuxinator .peco)
for dir in ${DOT_DIRS[@]}
do
    rm -rf $HOME/$dir
    ln -s $HOME/dotfiles/$dir/ $HOME/$dir
    echo 'ln '$HOME/$dir
done


# aspell setting
echo "lang en_US" > ~/.aspell.conf


# change login shell
for zsh_path in `cat /etc/shells | grep 'zsh'`; do
    echo "Do you set login sh as $zsh_path? [Y/n]"
    read ANSWER
    case $ANSWER in
        "" | "Y" | "y" )
            chsh -s /bin/zsh; break;;
        * ) ;;
    esac
done
echo "login shell: "$SHELL
