#! /bin/bash

set -eux

DIR=$(pwd)
SSHPASS=FILLMEIN

initialize() {
    mkdir -p ${DIR}/tmp
}

link_dotfiles() {
    ln -sf -t ~/ ${DIR}/.tmux.conf
    if [ -e ~/.emacs.d ] ; then
        rm -rf ~/.emacs.d
    fi
    ln -sf -t ~/ ${DIR}/.emacs.d/
    ln -sf -t ~/ ${DIR}/.agda
    ln -sf -t ~/ ${DIR}/.gitignore_global

    cp ${DIR}/.config/autostart/* ~/.config/autostart/
}

install_dependencies() {
	sudo pacman -Sy
  sudo pacman -S --noconfirm --needed base-devel yay manjaro-asian-input-support-ibus ibus-skk skk-jisyo vivaldi discord bitwarden steam code emacs tmux awesome-terminal-fonts opam the_silver_searcher xclip tailscale ledger-live-bin guake manjaro-printer avahi obsidian gimp inkscape agda agda-stdlib gtksourceview3 libreoffice-still libreoffice-still-ja lftp snapd jdk-openjdk openjdk-src nvm
  sudo pacman -S --noconfirm --needed texlive-latex texlive-latexrecommended texlive-latexextra texlive-fontsrecommended texlive-fontsextra texlive-mathscience texlive-langcjk texlive-luatex texlive-binextra

    yes | LANG=C yay --answerdiff None --answerclean None --mflags "--noconfirm" -S --needed slack-electron zoom dropbox nautilus-dropbox zotero otf-source-han-code-jp

    # might not be working
    # xdg-settings set default-web-browser vivaldi.desktop
}

# Note: This is dependent on link_dotfiles (emacs.d/init.el)
setup_agda() {
    agda-mode setup
}

setup_git() {
    git config --global user.name "Yuito Murase"
    git config --global user.email yuito@acupof.coffee
    git config --global core.editor "emacsclient -nw -a ''"
    git config --global core.excludesfile ~/.gitignore_global

    if [ ! -f ~/.ssh/id_ed25519 ]; then
        ssh-keygen -t ed25519 -C ${HOST} -f ~/.ssh/id_ed25519 -P $SSHPASS
    fi
}

setup_opam() {
    opam init -a
    eval $(opam env --switch=default)
    opam install -y ocaml-lsp-server merlin odoc ocamlformat utop ott

    # Coq
    opam install -y coq coqide

    # BER MetaOCaml
    if ! opam switch list | grep -q 4.14.1+BER; then
        opam switch create 4.14.1+BER -y
        opam install -y merlin utop
    fi

    # Use default switch
    opam switch default
    eval $(opam env --switch=default)
}

setup_scala() {
    if [ ! -f ${DIR}/tmp/cs ]; then
        # Copied from https://scala-lang.org/download/
        curl -fL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | gzip -d > tmp/cs
        chmod +x tmp/cs
    else
        echo "Skipping: Scala setup script is already downloaded"
    fi

    if which scalac; then
        echo "Skipping: Scala is already installed"
    else
        tmp/cs setup -y
    fi
}

setup_printer() {
    sudo systemctl enable --now cups.service
    sudo systemctl enable --now cups.socket
    sudo systemctl enable --now cups.path
    sudo systemctl enable --now avahi-daemon.service
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

blocking_setup_tailscale() {
    sudo systemctl enable tailscaled
    sudo tailscale up
}

initialize
install_dependencies
setup_git
setup_opam
setup_scala
setup_printer
setup_tmux
link_dotfiles
setup_agda
blocking_setup_tailscale
