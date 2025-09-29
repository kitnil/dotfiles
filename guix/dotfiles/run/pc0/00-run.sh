#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

iptables "-P" "FORWARD" "ACCEPT"
sysctl -w net.ipv4.conf.br0.forwarding=1

swapon /dev/vg0/swap

/home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/run/pc0/15-guix-nanokvm-networking-namespace.sh
/home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/run/pc0/15-guix-rde-networking-namespace.sh
/home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/run/pc0/15-guix-workstation-networking-namespace.sh
/home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/run/pc0/15-nixos-majordomo-networking-namespace.sh
/home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/run/pc0/15-nixos-workstation-networking-namespace.sh

mount -t tmpfs -o rw,relatime,size=100M,rshared none /mnt/guix-workstation/run
mount -t tmpfs -o rw,relatime,size=100M,rshared none /mnt/guix-workstation/tmp

mount /dev/vg0/guixworkstation /srv/runc/guix-workstation
mount /dev/vg0/nixosmajordomo /srv/runc/nixos-majordomo
mount /dev/vg0/guixnanokvm /srv/runc/guix-nanokvm

/home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/run/pc0/12-run-win10.sh

# /home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/run/pc0/03-net.sh

# rm /var/lib/kubelet/.maintenance
# herd restart kubelet
# /home/oleg/src/cgit.wugi.info/wigust/dotfiles/guix/dotfiles/run/pc0/13-guix-workstation-run.sh
