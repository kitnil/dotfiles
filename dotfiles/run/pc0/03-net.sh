#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

ip link add macvlan0 link eth0 type macvlan mode bridge
ip addr add 192.168.0.179/24 dev macvlan0
ip link set macvlan0 up
ip route add 192.168.0.178/32 dev macvlan0
