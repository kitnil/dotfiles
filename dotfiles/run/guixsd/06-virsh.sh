#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo virsh nodedev-detach pci_0000_12_00_0
sudo virsh nodedev-detach pci_0000_12_00_1
sudo virsh start win10
