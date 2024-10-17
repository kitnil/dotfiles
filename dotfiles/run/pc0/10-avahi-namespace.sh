#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo ip link add macvlan2 link eth0 type macvlan mode bridge
sudo ip link set macvlan2 netns "$NAMESPACE"
sudo ip netns exec "$NAMESPACE" ip link set macvlan2 up
sudo ip netns exec "$NAMESPACE" ip addr add 192.168.0.194/24 dev macvlan2
sudo ip netns exec "$NAMESPACE" ping -c 3 192.168.0.145
