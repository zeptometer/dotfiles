#! /bin/bash -eu

DIR=$(pwd)
read -p "Password: " -s pass; echo

link_dotfiles() {
    ln -sf ${DIR}/.tmux.conf ~/.tmux.conf
}

install_dependencies() {
    echo $pass | sudo -S apt update
    echo $pass | sudo -S apt upgrade
    install_tmux
}

install_tmux() {
    echo "# setting up tmux"
    echo $pass | sudo -S apt install tmux fonts-powerline

    local TPM_DIR=~/.tmux/plugins/tpm
    if ! [ -e $TPM_DIR ] ; then
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    else
        cd $TPM_DIR; git pull
    fi
}

install_dependencies
link_dotfiles
