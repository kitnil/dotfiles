#!/bin/sh

# mount -t 9p -o trans=virtio v_tmp /mnt

# -vga qxl \
# -spice port=5930,disable-ticketing \
# -device virtio-serial-pci \
# -device virtserialport,chardev=spicechannel0,name=com.redhat.spice.0 \

exec qemu-system-x86_64 \
     -enable-kvm \
     -net nic,model=virtio \
     -net user,hostfwd=tcp::12022-:22,hostfwd=tcp::12080-:80\
     -virtfs local,path="/nix/store",security_model=none,mount_tag="TAG_nix_store" \
     -chardev spicevmc,id=spicechannel0,name=vdagent \
     -drive file=/nixos-qemu-image,if=virtio,cache=writeback,werror=report \
     -m 4096 \
     -daemonize \
     -display none \
     "$@"
