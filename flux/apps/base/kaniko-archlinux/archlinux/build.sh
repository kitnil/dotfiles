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
    chromium
    firefox
)

packages+=(
    kubectl
)

packages+=(
    openbsd-netcat
    openssh
)

packages+=(
    aichat
)

packages+=(
    zenity
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
    sudo -u oleg -i bash <<'EOF'
set -o nounset -o errexit -o pipefail -o xtrace
git clone https://aur.archlinux.org/socialstreamninja.git
cd socialstreamninja || exit 1
sed -i 's/\r//g' PKGBUILD
makepkg --noconfirm
EOF
    pacman --noconfirm -U /home/oleg/socialstreamninja/socialstreamninja-0.3.43-1-x86_64.pkg.tar.zst
    rm -rf /home/oleg/socialstreamninja
}

install_aura()
{
    git clone https://aur.archlinux.org/aura-bin.git /usr/local/src/aura-bin
    chown -R oleg: /usr/local/src/aura-bin
    sudo -u oleg bash -c 'cd /usr/local/src/aura-bin || exit 1; makepkg --noconfirm -si'
}

aur()
{
    case "$1" in
        install)
            sudo -u oleg -i aura --noconfirm -A "$2"
            ;;
    esac
}

install_idea()
{
    aur install intellij-idea-community-edition
}

install_pycharm()
{
    aur install pycharm-community-edition
}

install_vscodium()
{
    aur install vscodium-bin
}

install_vscodium_extensions()
{
    sudo -u oleg -i bash <<'EOF'
curl  -L -o golang.Go-0.46.1.vsix https://marketplace.windsurf.com/api/golang/Go/0.46.1/file/golang.Go-0.46.1.vsix
codium --install-extension golang.Go-0.46.1.vsix
rm golang.Go-0.46.1.vsix

curl  -L -o sumneko.lua-3.14.0.vsix https://open-vsx.org/api/sumneko/lua/linux-x64/3.14.0/file/sumneko.lua-3.14.0@linux-x64.vsix
codium --install-extension sumneko.lua-3.14.0.vsix
rm sumneko.lua-3.14.0.vsix
EOF
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

install_dlv()
{
    GOBIN=/usr/local/bin go install github.com/go-delve/delve/cmd/dlv@latest
}

packages+=(
    lua51-luautf8
)
install_pob()
{
    aur install path-of-building-community-git
}

main()
{
    echo 'Server=https://archive.archlinux.org/repos/2025/06/06/$repo/os/$arch' > /etc/pacman.d/mirrorlist

    sed -i '/NoExtract/d' /etc/pacman.conf
    configure_locales
    pacman --noconfirm -Syu
    pacman --noconfirm -S "${packages[@]}"
    base_system
    locale-gen

    install_aura

    install_vscodium
    install_vscodium_extensions
    install_socialstream
    install_idea
    install_pycharm
    install_kubebuilder
    install_kind
    install_dlv
    install_pob
    aur install yandex-browser
}

main "$@"
