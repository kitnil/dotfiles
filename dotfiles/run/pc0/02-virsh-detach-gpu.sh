#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo virsh nodedev-detach pci_0000_03_00_0
sudo virsh nodedev-detach pci_0000_03_00_1
