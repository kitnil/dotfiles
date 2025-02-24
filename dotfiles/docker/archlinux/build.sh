#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

packages=(
    alsa-lib
    atk
    bash-completion
    cups
    fuse-overlayfs
    fuse2
    gdk-pixbuf2
    gtk3
    less
    nss
    pulseaudio
    strace
    sudo
    tmux
    unzip
    wmenu
    xorg-xwayland
)

packages+=(
    glibc
)

packages+=(
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    noto-fonts-extra
)

packages+=(
    base-devel
    git
)

packages+=(
    git-grab
)

packages+=(
    libxtst
)

packages+=(
    foot
)

packages+=(
    nano
)

packages+=(
    go
)

packages+=(
    firefox
)

packages+=(
    kubectl
)

packages+=(
    openssh
)

base_system()
{
    groups=(
        video
        wheel
        kmem
    )
    useradd -m oleg
    passwd --delete oleg
    for group in "${groups[@]}"
    do
        gpasswd -a oleg "$group"
    done

    sed -i 's|# %wheel ALL=(ALL:ALL) NOPASSWD: ALL|%wheel ALL=(ALL:ALL) NOPASSWD: ALL|' /etc/sudoers

    cat >> /home/oleg/.bashrc <<'EOF'

systemctl()
{
    XDG_RUNTIME_DIR="/run/user/${UID}" command systemctl "$@"
}
EOF
}

configure_locales()
{
    cat >> /etc/locale.gen <<'EOF'

ru_RU.KOI8-R KOI8-R
ru_RU.UTF-8 UTF-8
ru_RU ISO-8859-5
EOF
}

install_socialstream()
{
    curl -o /tmp/socialstream-linux-AppImage.zip https://iso.wugi.info/socialstream-0.1.52-linux-AppImage.zip

    cd /usr/local/bin
    unzip /tmp/socialstream-linux-AppImage.zip
    rm /tmp/socialstream-linux-AppImage.zip
    mv socialstream-0.1.52-x86_64.AppImage socialstream
    chmod 555 socialstream
}

install_yay()
{
    git clone https://aur.archlinux.org/yay-bin.git /usr/local/src/yay-bin
    chown -R oleg: /usr/local/src/yay-bin
    sudo -u oleg bash -c 'cd /usr/local/src/yay-bin || exit 1; makepkg --noconfirm -si'
}

install_idea()
{
    yay --noconfirm -S intellij-idea-community-edition
}

install_pycharm()
{
    yay --noconfirm -S pycharm-community-edition
}

install_vscode()
{
    yay --noconfirm -S vscode
}

install_vscode_extensions()
{
    local extensions=(
        golang.go
        sumneko.lua
    )
    for extension in "${extensions[@]}"
    do
        sudo -u oleg -i code --install-extension "$extension"
    done
}

install_kubebuilder()
{
    curl -L -o /usr/local/bin/kubebuilder "https://go.kubebuilder.io/dl/latest/$(go env GOOS)/$(go env GOARCH)"
    chmod +x /usr/local/bin/kubebuilder
}

install_kind()
{
    GOBIN=/usr/local/bin go install sigs.k8s.io/kind@v0.26.0
}

packages+=(
    docker
)
install_docker()
{
    systemctl enable docker.service
    gpasswd -a oleg docker
}

packages+=(
    lua51-luautf8
)
install_pob()
{
    yay --noconfirm -S path-of-building-community-git
}

main()
{
    sed -i '/NoExtract/d' /etc/pacman.conf
    configure_locales
    pacman --noconfirm -Syu
    pacman --noconfirm -S "${packages[@]}"
    base_system
    locale-gen

    install_docker
    install_socialstream
    install_yay
    install_idea
    install_pycharm
    install_vscode
    install_vscode_extensions
    install_kubebuilder
    install_kind
    install_pob
}

main "$@"
