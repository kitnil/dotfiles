#!/usr/bin/env bash

set -ex

echo nameserver 8.8.8.8 > /etc/resolv.conf
dnf install systemd
dnf install procps
dnf install libvirtd
dnf install qemu-kvm
dnf install nano
echo proxy=socks5://192.168.0.148:9050 > /etc/dnf/dnf.conf
dnf install qemu-kvm qemu-img virt-manager libvirt virt-install virt-viewer

virsh define win11-pc0.xml

cat <<'EOF'
mknod -m 600 /dev/mapper/vg0-ntfsgames b 253 4
mknod -m 660 /dev/dm-16 b 253 16
mknod -m 660 /dev/dm-0 b 253 0
mknod -m 600 /dev/mapper/vg0-win10 b 253 3

chmod 0600 /dev/vfio/15 /dev/vfio/14

mount -t hugetlbfs hugetlbfs /hugepages
systemctl restart libvirtd
virsh net-start default
EOF
