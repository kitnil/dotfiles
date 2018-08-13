#!/bin/sh

image=$1
qcow=$2

# Create mount direcroty
echo mkdir /tmp/$image

# Load kernel module
echo modprobe nbd

# Mount QEMU image
echo qemu-nbd -c /dev/nbd0 /srv/virt/$qcow
echo mount /dev/nbd0p1 /tmp/$image

# Mouch chroot requisites
echo mount --bind /dev /tmp/$image/dev
echo mount --bind /dev/pts /tmp/$image/dev/pts
echo mount --bind /dev/shm /tmp/$image/dev/shm
echo mount --bind /srv/share /tmp/$image/mnt
echo mount --bind /tmp /tmp/$image/tmp
echo mount --bind $XDG_RUNTIME_DIR /tmp/$image/var/run/user/1000
echo mount -t proc none /tmp/$image/proc
echo mount -t sysfs sys /tmp/$image/sys
