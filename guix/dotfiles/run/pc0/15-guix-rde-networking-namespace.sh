#!/bin/sh
ip netns add guix-rde
ip link add name guix2 type veth peer name guix3
ip link set dev guix3 netns guix-rde
ip netns exec guix-rde ip link set guix3 name eth0
ip netns exec guix-rde ip link set eth0 up
ip link set guix2 master br0
ip link set guix2 up
ip netns exec guix-rde ip addr add 192.168.0.193/24 dev eth0
ip netns exec guix-rde ip route add default via 192.168.0.1
