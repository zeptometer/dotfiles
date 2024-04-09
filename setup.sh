#! /bin/bash

set -eux

DIR=$(pwd)
HOSTNAME=FILLTHISIN
SSHPASS=FILLTHISIN

link_dotfiles() {
    ln -sf -t ~/ ${DIR}/.tmux.conf
    if [ -e ~/.emacs.d ] ; then
        rm -rf ~/.emacs.d
    fi
    ln -sf -t ~/ ${DIR}/.emacs.d/
}

install_dependencies_ubuntu() {
    if find /etc/apt/ -name *.list | xargs cat | grep  ^[[:space:]]*deb | grep -q linrunner/tlp; then
        echo "# tlp repo is alreday added"
    else
        echo "# Adding tlp repo"
        sudo add-apt-repository -y ppa:linrunner/tlp
    fi
    sudo apt update
    sudo apt -y upgrade
    sudo apt install -y tmux fonts-powerline git build-essential uim-skk guake python3-gpg steam xclip silversearcher-ag texlive-full opam gnome-tweaks tlp

    sudo snap install emacs --classic
    sudo snap install code --classic
    sudo snap install bitwarden
    sudo snap install zoom-client

    rm -rf tmp_deb
    mkdir tmp_deb
    cd tmp_deb

    if dpkg -s discord >/dev/null 2>&1; then
        echo "# Discord is already installed."
    else
        echo "# Installing Discord"
        wget -O discord.deb "https://discord.com/api/download?platform=linux&format=deb"
        sudo dpkg -i discord.deb
    fi


    if dpkg -s vivaldi-stable >/dev/null 2>&1; then
        echo "# Vivaldi is already installed."
    else
        echo "# Installing Vivaldi"
        wget -O vivaldi.deb https://downloads.vivaldi.com/stable/vivaldi-stable_amd64.deb
        sudo dpkg -i ./vivaldi.deb
        # Set vivaldi as default web browser
        xdg-settings set default-web-browser vivaldi.desktop
    fi

    if dpkg -s dropbox >/dev/null 2>&1; then
        echo "# Dropbox is alredy installed."
    else
	echo "# Installing Dropbox"
        wget -O dropbox.deb https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2020.03.04_amd64.deb
        sudo dpkg -i dropbox.deb
    fi

    echo "For Slack, you would need to install it manually (not from Snap!)"
}

setup_git() {
    git config --global user.name "Yuito Murase"
    git config --global user.email yuito@acupof.coffee

    if [ ! -f ~/.ssh/id_ed25519 ]; then
        ssh-keygen -t ed25519 -C ${HOSTNAME} -f ~/.ssh/id_ed25519 -P $SSHPASS
    fi
}

setup_tmux() {
    echo "# Setting up tmux"

    local TPM_DIR=~/.tmux/plugins/tpm
    if ! [ -e $TPM_DIR ] ; then
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    else
        cd $TPM_DIR; git pull
    fi
}

setup_opam() {
    opam init -a
    eval $(opam env --switch=default)
    opam install -y ocaml-lsp-server odoc ocamlformat utop ott
}

install_dependencies_ubuntu
setup_git
setup_tmux
setup_opam
link_dotfiles
