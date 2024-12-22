#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

packages=(
    bash-completion
    foot
    less
    procps
    pulseaudio
    strace
    sudo
    systemd
    tmux
    udev
    xwayland
)

packages+=(
    kali-linux-headless
)

main()
{
    apt update
    apt install -y "${packages[@]}"

    useradd -m oleg 
    passwd --delete oleg 
    gpasswd -a oleg video 
    gpasswd -a oleg sudo 
    chsh --shell /bin/bash oleg

    sed -i 's|# %wheel ALL=(ALL:ALL) NOPASSWD: ALL|%wheel ALL=(ALL:ALL) NOPASSWD: ALL|' /etc/sudoers
}

main "$@"
