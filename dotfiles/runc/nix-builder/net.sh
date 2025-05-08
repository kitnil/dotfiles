#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

ip link add name veth-host-nix type veth peer name veth-guest-nix
ip link set veth-host-nix up
brctl addif runc0 veth-host-nix
ip netns add runc-nix
ip link set veth-guest-nix netns runc-nix
ip netns exec runc-nix ip link set veth-guest-nix name eth1
ip netns exec runc-nix ip addr add 192.168.10.102/24 dev eth1
ip netns exec runc-nix ip link set eth1 up
ip netns exec runc-nix ip route add default via 192.168.10.1
