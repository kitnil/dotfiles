#!/bin/sh
ip netns add nixos-workstation
ip link add name nixos0 type veth peer name nixos1
ip link set dev nixos1 netns nixos-workstation
ip netns exec nixos-workstation ip link set nixos1 name eth0
ip netns exec nixos-workstation ip link set eth0 up
ip link set nixos0 master br0
ip link set nixos0 up
ip netns exec nixos-workstation ip addr add 192.168.0.195/24 dev eth0
ip netns exec nixos-workstation ip route add default via 192.168.0.1
