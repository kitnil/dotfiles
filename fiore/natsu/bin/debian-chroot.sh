#!/bin/sh

# Create mount direcroty
mkdir /tmp/debian

# Load kernel module
modprobe nbd

# Mount QEMU image
qemu-nbd -c /dev/nbd0 /srv/virt/debian-gnome.img
mount /dev/nbd0p1 /tmp/debian

# Mouch chroot requisites
mount --bind /dev/pts /tmp/debian/dev/pts
mount --bind $XDG_RUNTIME_DIR /tmp/debian/var/run/user/1000
mount --bind /dev/shm /tmp/debian/dev/shm
mount --bind /srv/share /tmp/debian/mnt
mount --bind /tmp /tmp/debian/tmp
mount --bind /dev /tmp/debian/dev
mount -t proc none /tmp/debian/proc
mount -t sysfs sys /tmp/debian/sys
