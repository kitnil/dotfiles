#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/02-virsh-detach-gpu.sh
/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/07-video.sh
/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/08-lg.sh
virsh start win10
