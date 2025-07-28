#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

swapon /dev/vg0/swap

/home/oleg/.local/share/chezmoi/guix/dotfiles/run/pc0/12-run-win10.sh
# /home/oleg/.local/share/chezmoi/dotfiles/run/pc0/03-net.sh

rm /var/lib/kubelet/.maintenance
herd restart kubelet
# /home/oleg/.local/share/chezmoi/dotfiles/run/pc0/13-guix-workstation-run.sh
