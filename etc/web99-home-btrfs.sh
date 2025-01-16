#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

lvcreate -L 45G -n webbtrfs vg0
mkfs.btrfs -f -L webbtrfs /dev/vg0/webbtrfs

mkdir -p /mnt/web-btrfs
mount -o compress=zstd:3,ssd,noatime,degraded /dev/vg0/webbtrfs /mnt/web-btrfs
btrfs quota enable /mnt/web-btrfs
btrfs subvolume create /mnt/web-btrfs/web99-home
umount /mnt/web-btrfs
