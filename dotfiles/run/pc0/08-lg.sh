#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

cp /root/qemu.conf /etc/libvirt/qemu.conf
herd stop libvirtd
herd start libvirtd

modprobe kvmfr static_size_mb=128
chown oleg:kvm /dev/kvmfr0
