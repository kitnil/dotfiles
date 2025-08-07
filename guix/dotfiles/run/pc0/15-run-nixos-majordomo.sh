#!/bin/sh
ip netns add nixos-majordomo
ip link add name nixos2 type veth peer name nixos3
ip link set dev nixos3 netns nixos-majordomo
ip netns exec nixos-majordomo ip link set nixos3 name eth0
ip netns exec nixos-majordomo ip link set eth0 up
ip link set nixos2 master br0
ip link set nixos2 up
ip netns exec nixos-majordomo ip addr add 192.168.0.197/24 dev eth0
ip netns exec nixos-majordomo ip route add default via 192.168.0.1
