#!/bin/sh
ip netns add guix-nanokvm
ip link add name guix4 type veth peer name guix5
ip link set dev guix5 netns guix-nanokvm
ip netns exec guix-nanokvm ip link set guix5 name eth0
ip netns exec guix-nanokvm ip link set eth0 up
ip link set guix4 master br0
ip link set guix4 up
ip netns exec guix-nanokvm ip addr add 192.168.0.198/24 dev eth0
ip netns exec guix-nanokvm ip route add default via 192.168.0.1
