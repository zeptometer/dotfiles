#! /bin/bash -eux

DIR=$(pwd)

link_dotfiles() {
    ln -sf -t ~/ ${DIR}/.tmux.conf
    if [ -e ~/.emacs.d ] ; then
        rm -rf ~/.emacs.d
    fi
    ln -sf -t ~/ ~/ ${DIR}/.emacs.d/
}

install_dependencies_ubuntu() {
    sudo apt update
    sudo apt -y upgrade
    sudo apt install -y tmux fonts-powerline
}

install_tpm() {
    echo "# setting up tpm"

    local TPM_DIR=~/.tmux/plugins/tpm
    if ! [ -e $TPM_DIR ] ; then
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    else
        cd $TPM_DIR; git pull
    fi
}

postprocess() {
    tmux source-file ~/.tmux.conf
}

# install_dependencies_ubuntu
link_dotfiles
postprocess
