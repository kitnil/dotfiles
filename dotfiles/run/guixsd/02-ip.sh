#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

ip link set br154 up

ip link set br155-vlan155 up
# ip address add 127.0.0.3/8 dev 127.0.0.4/8

ip link set br154.154 up
ip address add 192.168.154.1/24 dev br154.154

ip link set br156 up
ip address add 127.0.0.156/8 dev br156

ip link set br156.156 up
ip address add 192.168.156.1/24 dev br156.156

modprobe netconsole netconsole=@/enp34s0,6666@192.168.0.194/

echo sudo -i /gnu/store/l2dw3lfb2qxjp6bgrn431fxa428rgs1f-dnsmasq-2.90/sbin/dnsmasq --no-daemon --local-service --interface=enp34s0 --ipset=/play.google.com/tor --ipset=/youtube.com/googlevideo.com/byedpi --server=8.8.8.8 --no-resolv --bind-interfaces --except-interface=lo --except-interface=br154.br154 --except-interface=br0

sudo ip route add 192.168.25.11 via 192.168.25.1 # znc
sudo ip route add 192.168.25.12 via 192.168.25.1

sudo ipset -exist create tor hash:ip hashsize 1024 maxelem 655360
sudo iptables -t nat -A PREROUTING -p tcp -m set --match-set tor dst -j DNAT --to-destination 127.0.0.1:888
sudo iptables -t nat -A OUTPUT -p tcp -m set --match-set tor dst -j REDIRECT --to-ports 888

# yt3.ggpht.com
sudo ipset add tor 64.233.162.198

# rutracker
sudo ipset add tor 172.67.182.196
sudo ipset add tor 104.21.32.39
