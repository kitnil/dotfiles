#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

echo "0000:03:00.0" > /sys/bus/pci/drivers/vfio-pci/unbind
echo 3 > /sys/bus/pci/devices/0000:03:00.0/resource2_resize
echo > /sys/bus/pci/devices/0000:03:00.0/reset_method

virsh nodedev-detach pci_0000_03_00_0
virsh nodedev-detach pci_0000_03_00_1
