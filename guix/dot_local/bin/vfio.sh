#!/usr/bin/env bash

set -ex

echo '1002 7340' > /sys/bus/pci/drivers/vfio-pci/new_id
echo 0000:12:00.0 > /sys/bus/pci/devices/0000:12:00.0/driver/unbind
echo 0000:12:00.0 > /sys/bus/pci/drivers/vfio-pci/bind
echo '1002 7340' > /sys/bus/pci/drivers/vfio-pci/remove_id
lspci -vnn -d 1002:7340

echo '1002 ab38' > /sys/bus/pci/drivers/vfio-pci/new_id
echo 0000:12:00.1 > /sys/bus/pci/devices/0000:12:00.1/driver/unbind
echo 0000:12:00.1 > /sys/bus/pci/drivers/vfio-pci/bind
echo '1002 ab38' > /sys/bus/pci/drivers/vfio-pci/remove_id
lspci -vnn -d 1002:ab38

