#!/bin/sh

# Run qemu-system-x86_64 with KVM and bridge
# Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
# Released under the GNU GPLv3 or any later version.

qemu-system-x86_64 \
    -smp cores=4,threads=1 \
    -m 4096 \
    -enable-kvm \
    -cpu host \
    -daemonize \
    -net nic,model=virtio,macaddr="52:54:a8:2c:f2:22" -net bridge,br=br0 \
    -vnc :5 \
    -hda /home/oleg/vm/debian.qcow2 \
    -boot order=c \
    "$@"
