#!/bin/sh

# Run qemu-system-x86_64 with KVM and bridge
# Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
# Released under the GNU GPLv3 or any later version.

# Make sure /etc/qemu/bridge.conf contains ‘allow br0’.

# -machine type=pc,accel=kvm
# -drive file=ubuntu.iso,format=raw,aio=native,cache=none
#
#-M q35 \
#
#    -net nic,model=virtio -net user,hostfwd=tcp::2222-:22 \
#    -virtfs local,path="/srv/share",security_model=none,mount_tag="TAG_share" \
#
# exec qemu-system-x86_64 \
#      -smp 2 \
#      -m 4096 \
#      -enable-kvm \
#      -M q35 \
#      -net nic -net bridge,br=br0 \
#      $@
#
#     -usb \
#     -device usb-host,hostbus=1,hostaddr=11 \

# https://superuser.com/questions/132322/how-to-increase-the-visualized-screen-resolution-on-qemu-kvm
# You can toggle fullscreen with Ctrl + Alt + F, or by passing -full-screen.

# https://wiki.archlinux.org/index.php/QEMU#Creating_bridge_manually
printf -v macaddr "52:54:%02x:%02x:%02x:%02x" $(( $RANDOM & 0xff)) $(( $RANDOM & 0xff )) $(( $RANDOM & 0xff)) $(( $RANDOM & 0xff ))

qemu-system-x86_64 \
    -smp cores=4,threads=1 \
    -m 4096 \
    -enable-kvm \
    -cpu host \
    -daemonize \
    -net nic,macaddr="$macaddr" -net bridge,br=br0 \
    -vga virtio \
    -full-screen \
    "$@"
