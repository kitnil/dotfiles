#!/bin/sh

image='/tmp/games'
qcow="$HOME/vm/games.qcow2"

sudo modprobe nbd
mkdir -p "$image"

sudo qemu-nbd -c /dev/nbd0 "$qcow"
sudo vgscan
sudo vgchange -ay

sudo mount /dev/debian-vg/root "$image"

sudo mount --bind /dev "$image/dev"
sudo mount --bind /dev/pts "$image/dev/pts"
sudo mount --bind /dev/shm "$image/dev/shm"
sudo mount --bind /srv/share "$image/mnt"
sudo mount --bind /tmp "$image/tmp"
sudo mount --bind /run/user/1000 "$image/var/run/user/1000"
sudo mount -t proc none "$image/proc"
sudo mount -t sysfs sys "$image/sys"

xhost +local:

sudo chroot "$image" /bin/bash
# source /etc/profile
# su - natsu
