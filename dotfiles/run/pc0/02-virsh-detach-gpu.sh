#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

virsh nodedev-detach pci_0000_03_00_0
virsh nodedev-detach pci_0000_03_00_1
