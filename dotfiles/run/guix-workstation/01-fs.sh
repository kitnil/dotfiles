#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

mkdir /home/oleg/.config/google-chrome
chown oleg: /home/oleg/.config/google-chrome
mount --bind /mnt/home/oleg/.config/google-chrome /home/oleg/.config/google-chrome
mount -o rw,remount /home/oleg/.config/google-chrome

mkdir /nix
mount --bind /mnt/nix /nix

mkdir /home/oleg/.mozilla
chown oleg: /home/oleg/.mozilla
mount --bind /mnt/home/oleg/.mozilla /home/oleg/.mozilla
mount -o rw,remount /home/oleg/.mozilla

mkdir /home/oleg/.config/obs-studio
chown oleg: /home/oleg/.config/obs-studio
mount --bind /mnt/home/oleg/.config/obs-studio /home/oleg/.config/obs-studio
mount -o rw,remount /home/oleg/.config/obs-studio

mkdir -p /home/oleg/.local/share/remmina
chown oleg: /home/oleg/.local/share /home/oleg/.local/share/remmina
mount --bind /mnt/home/oleg/.local/share/remmina /home/oleg/.local/share/remmina
mount -o rw,remount /home/oleg/.local/share/remmina

mkdir -p /home/oleg/.password-store
chown oleg: /home/oleg/.local/share /home/oleg/.password-store
mount --bind /mnt/home/oleg/.password-store /home/oleg/.password-store
mount -o rw,remount /home/oleg/.password-store

mkdir -p /home/oleg/.gnupg
chown oleg: /home/oleg/.local/share /home/oleg/.gnupg
mount --bind /mnt/home/oleg/.gnupg /home/oleg/.gnupg
mount -o rw,remount /home/oleg/.gnupg

mkdir -p /home/oleg/.local/share/TelegramDesktop
chown oleg: /home/oleg/.local/share /home/oleg/.local/share/TelegramDesktop
mount --bind /mnt/home/oleg/.local/share/TelegramDesktop /home/oleg/.local/share/TelegramDesktop
mount -o rw,remount /home/oleg/.local/share/TelegramDesktop

mkdir -p /home/oleg/.cache/ihs
chown oleg: /home/oleg/.local/share /home/oleg/.cache/ihs
mount --bind /mnt/home/oleg/.cache/ihs /home/oleg/.cache/ihs
mount -o rw,remount /home/oleg/.cache/ihs
