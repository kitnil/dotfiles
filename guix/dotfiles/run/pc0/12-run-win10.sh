#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

/home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/run/pc0/07-video.sh
/home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/run/pc0/08-lg.sh
modprobe vfio_iommu_type1
virsh start win10
