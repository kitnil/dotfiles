#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo virsh nodedev-detach pci_0000_12_00_0
sudo virsh nodedev-detach pci_0000_12_00_1
sudo virsh start win10

cat <<'EOF'
It's possible to reattach devices, e.g.:

root@guixsd ~# virsh nodedev-reattach pci_0000_12_00_0
Device pci_0000_12_00_0 re-attached

root@guixsd ~# virsh nodedev-reattach pci_0000_12_00_1
Device pci_0000_12_00_1 re-attached
EOF
