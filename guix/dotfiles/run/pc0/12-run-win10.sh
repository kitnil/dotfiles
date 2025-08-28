#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

/home/oleg/.local/share/chezmoi/guix/dotfiles/run/pc0/07-video.sh
/home/oleg/.local/share/chezmoi/guix/dotfiles/run/pc0/08-lg.sh
modprobe vfio_iommu_type1
virsh start win10
