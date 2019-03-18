#! /bin/bash -eux

DIR=$(pwd)

link_dotfiles() {
    ln -sf -t ~/ ${DIR}/.tmux.conf
    if [ -e ~/.emacs.d ] ; then
        rm -rf ~/.emacs.d
    fi
    ln -sf -t ~/ ~/ ${DIR}/.emacs.d/
}

install_dependencies() {
    sudo apt update
    sudo apt -y upgrade
    install_tmux
}

install_tmux() {
    echo "# setting up tmux"
    sudo apt install -y tmux fonts-powerline

    local TPM_DIR=~/.tmux/plugins/tpm
    if ! [ -e $TPM_DIR ] ; then
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    else
        cd $TPM_DIR; git pull
    fi
}

install_dependencies
link_dotfiles
