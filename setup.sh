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
    if find /etc/apt/ -name *.list | xargs cat | grep  ^[[:space:]]*deb | grep -q tailscale; then
        echo "# TailScale repo is alreday added"
    else
        echo "# Adding TailScale repo"
        # Copied from https://tailscale.com/download/linux/ubuntu-2204
        curl -fsSL https://pkgs.tailscale.com/stable/ubuntu/jammy.noarmor.gpg | sudo tee /usr/share/keyrings/tailscale-archive-keyring.gpg >/dev/null
        curl -fsSL https://pkgs.tailscale.com/stable/ubuntu/jammy.tailscale-keyring.list | sudo tee /etc/apt/sources.list.d/tailscale.list
    fi
    if find /etc/apt/ -name *.list | xargs cat | grep  ^[[:space:]]*deb | grep -q appimagelauncher; then
        echo "# appimagelauncher repo is alreday added"
    else
        echo "# Adding appimagelauncher repo"
        # Copied from https://support.ledger.com/hc/ja/articles/4404389606417-Ledger-Live%E3%82%92%E3%83%80%E3%82%A6%E3%83%B3%E3%83%AD%E3%83%BC%E3%83%89-%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB?docs=true
        sudo add-apt-repository -y ppa:appimagelauncher-team/stable
    fi

    sudo apt update
    sudo apt -y upgrade
    sudo apt install -y tmux fonts-powerline git build-essential uim-skk guake python3-gpg steam xclip silversearcher-ag opam gnome-tweaks tlp tailscale software-properties-common appimagelauncher

    sudo snap install emacs --classic
    sudo snap install code --classic
    sudo snap install bitwarden
    sudo snap install miro
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

blocking_setup_ledger() {
    # Copied from https://support.ledger.com/hc/ja/articles/4404389606417-Ledger-Live%E3%82%92%E3%83%80%E3%82%A6%E3%83%B3%E3%83%AD%E3%83%BC%E3%83%89-%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB?docs=true
    mkdir -p tmp/ledger
    if [ ! -f tmp/ledger/add_udev_rules.sh ]; then
        wget -q -O tmp/ledger/add_udev_rules.sh https://raw.githubusercontent.com/LedgerHQ/udev-rules/master/add_udev_rules.sh
        sudo bash tmp/ledger/add_udev_rules.sh
    fi
    if find /etc/apt/ -name *.list | xargs cat | grep  ^[[:space:]]*deb | grep -q universe; then
        echo "# universe repo is alreday added"
    else
        echo "# Adding universe repo"
        # Copied from https://support.ledger.com/hc/ja/articles/4404389606417-Ledger-Live%E3%82%92%E3%83%80%E3%82%A6%E3%83%B3%E3%83%AD%E3%83%BC%E3%83%89-%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB?docs=true
        sudo add-apt-repository -y universe
    fi
    sudo apt install libfuse2

    if [ ! -f tmp/ledger/install-appimage-done ]; then
        echo "# Installing Ledger Live Desktop AppImage"
        wget -O tmp/ledger/ledger-live-desktop.AppImage https://download.live.ledger.com/latest/linux
        chmod +x tmp/ledger/ledger-live-desktop.AppImage
        ./tmp/ledger/ledger-live-desktop.AppImage
        touch tmp/ledger/install-appimage-done
    fi
}

blocking_setup_tailscale() {
    sudo tailscale up
}

# Context: https://askubuntu.com/questions/956006/pregenerating-context-markiv-format-this-may-take-some-time-takes-forever
blocking_install_texlive() {
    sudo apt-get install texlive-full
}

install_dependencies_ubuntu
setup_git
setup_tmux
setup_opam
link_dotfiles
# potentially blocking operations that require user input
blocking_install_texlive
blocking_setup_tailscale
blocking_setup_ledger
