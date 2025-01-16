#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

lvcreate -L 45G -n web99homebtrfs vg0
mkfs.btrfs -L web99homebtrfs /dev/vg0/web99homebtrfs

mkdir -p /mnt/web99-home-btrfs
mount -o compress=zstd:3,ssd,noatime,degraded /dev/vg0/web99homebtrfs /mnt/web99-home-btrfs
btrfs quota enable /mnt/web99-home-btrfs
btrfs subvolume create /mnt/web99-home-btrfs
umount /mnt/web99-home-btrfs
