#!/bin/sh

exec qemu-system-x86_64 \
     -daemonize \
     -m 4096 \
     -smp 2 \
     -enable-kvm \
     -hda /srv/virt/epson.qcow2 \
     -smb /srv/share \
     -M q35 \
     -usb \
     -usbdevice host:04b8:0810 \
     -net user,hostfwd=tcp::13389-:3389,hostfwd=tcp::13445-:445 \
     -net nic \
     $@
