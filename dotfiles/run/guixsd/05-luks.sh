#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

/home/oleg/.local/share/chezmoi/dot_local/bin/executable_luks-decrypt.sh

sudo swapon /dev/lvm2/swap

sudo -i bash -c 'sysctl vm.nr_hugepages=4096; mount -t hugetlbfs hugetlbfs /hugepages; herd stop libvirtd; herd start libvirtd'
sudo -i bash -c 'lvchange -ay /dev/lvm1/win10; lvchange -ay /dev/lvm2/ntfsgames'

echo sudo -i /gnu/store/l2dw3lfb2qxjp6bgrn431fxa428rgs1f-dnsmasq-2.90/sbin/dnsmasq --no-daemon --local-service --interface=enp34s0 --ipset=/play.google.com/tor --ipset=/youtube.com/googlevideo.com/byedpi --server=8.8.8.8 --no-resolv --bind-interfaces --except-interface=lo --except-interface=br154.br154 --except-interface=br0

sudo ip route add 192.168.25.11 via 192.168.25.1 # znc
sudo ip route add 192.168.25.12 via 192.168.25.1

sudo ipset -exist create tor hash:ip hashsize 1024 maxelem 655360
sudo iptables -t nat -A PREROUTING -p tcp -m set --match-set tor dst -j DNAT --to-destination 127.0.0.1:888
sudo iptables -t nat -A OUTPUT -p tcp -m set --match-set tor dst -j REDIRECT --to-ports 888

sudo ipset -exist create byedpi hash:ip hashsize 1024 maxelem 655360
sudo iptables -t nat -A PREROUTING -p tcp -m set --match-set byedpi dst -j DNAT --to-destination 127.0.0.1:890
sudo iptables -t nat -A OUTPUT -p tcp -m set --match-set byedpi dst -j REDIRECT --to-ports 890

# yt3.ggpht.com
sudo ipset add tor 64.233.162.198

# rutracker
sudo ipset add tor 172.67.182.196
sudo ipset add tor 104.21.32.39

sudo mv /run/setuid-programs/mount.nfs /run/setuid-programs/mount.nfs.1

sudo modprobe drbd9
sudo modprobe netconsole netconsole=@/br0,6666@192.168.0.192/

sudo herd start vncserver2
