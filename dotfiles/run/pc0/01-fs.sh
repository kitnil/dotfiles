#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

mount /dev/vg0/data1 /mnt
mount --bind /mnt/home/oleg/.var/app/ru.linux_gaming.PortProton /home/oleg/.var/app/ru.linux_gaming.PortProton
mount --bind /mnt/home/oleg/.local/share/guix-sandbox-home/.local/share/Steam /home/oleg/.local/share/guix-sandbox-home/.local/share/Steam
umount /mnt
