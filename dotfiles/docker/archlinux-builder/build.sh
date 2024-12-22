#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

packages+=(
    base-devel
    git
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
}

install_yay()
{
    git clone https://aur.archlinux.org/yay-bin.git /usr/local/src/yay-bin
    chown -R oleg: /usr/local/src/yay-bin
    sudo -u oleg bash -c 'cd /usr/local/src/yay-bin || exit 1; makepkg --noconfirm -si'
}

main()
{
    pacman --noconfirm -Syu
    pacman --noconfirm -S "${packages[@]}"
    base_system

    install_yay
}

main "$@"
