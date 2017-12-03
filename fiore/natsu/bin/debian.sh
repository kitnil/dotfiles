#!/bin/sh

# -net nic -net bridge,br=bridge0

exec qemu-system-x86_64 \
     -daemonize \
     -m 4096 \
     -enable-kvm \
     -hda /srv/virt/debian-gnome.img \
     -virtfs local,path="/home/natsu/Documents",security_model=none,mount_tag="TAG_doc" \
     $@
# -cdrom /srv/archive/debian/debian-live-9.1.0-amd64-gnome.iso -boot order=d
