#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

/home/oleg/.local/share/chezmoi/guix/dot_local/bin/luks-decrypt.sh

sudo swapon /dev/lvm2/swap

sudo -i bash -c 'lvchange -ay /dev/lvm1/win10; lvchange -ay /dev/lvm2/ntfsgames'

sudo modprobe drbd9

sudo mv /run/setuid-programs/mount.nfs /run/setuid-programs/mount.nfs.1
