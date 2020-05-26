#!/bin/sh

# -device usb-host,hostbus=2,hostaddr=4 \
# -net 'user,hostfwd=tcp::3389-:3389,hostfwd=tcp::445-:445' \
# -net nic \
# -usb \

exec -a "$0" qemu-system-x86_64 \
     -daemonize \
     -m 4096 \
     -smp 2 \
     -enable-kvm \
     -hda /srv/virt/epson.qcow2 \
     -M q35 \
     -net nic -net user,smb=/srv/share \
     "$@"
