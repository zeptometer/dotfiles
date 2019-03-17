#! /bin/bash -eu

DIR=$(pwd)

link_dotfiles() {
    ln -sf -t ~/ ${DIR}/.tmux.conf
    if [ -e ~/.emacs.d ] ; then
        rm -rf ~/.emacs.d
    fi
    ln -sf -t ~/ ~/ ${DIR}/.emacs.d/
}

install_dependencies() {
    sudo -S apt update
    sudo -S apt upgrade
    install_tmux
}

install_tmux() {
    echo "# setting up tmux"
    sudo -S apt install tmux fonts-powerline

    local TPM_DIR=~/.tmux/plugins/tpm
    if ! [ -e $TPM_DIR ] ; then
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    else
        cd $TPM_DIR; git pull
    fi
}

install_dependencies
link_dotfiles
