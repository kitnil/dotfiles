#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/12-run-win10.sh

rm /var/lib/kubelet/.maintenance
herd restart kubelet
/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/13-guix-workstation-run.sh
