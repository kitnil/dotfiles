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
    libxtst
)

packages+=(
    foot
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

main()
{
    pacman --noconfirm -Syu
    pacman --noconfirm -S "${packages[@]}"
    base_system

    install_socialstream
    install_yay
    install_idea
}

main "$@"
