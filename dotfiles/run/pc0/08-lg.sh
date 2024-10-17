#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo cp /root/qemu.conf /etc/libvirt/qemu.conf
sudo herd stop libvirtd
sudo herd start libvirtd

sudo modprobe kvmfr static_size_mb=128
sudo chown oleg:kvm /dev/kvmfr0
# sudo chmod 0660 /dev/kvmfr0
