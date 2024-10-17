#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

ip netns add ns1
ip link set macvlan1 netns ns1
ip netns exec ns1 ip link set macvlan1 up
ip netns exec ns1 ip addr add 192.168.0.193/24 dev macvlan1
ip netns exec ns1 ip route add default via 192.168.0.253
ip netns exec ns1 ping -c 3 192.168.0.253
