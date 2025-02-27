#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

NAMESPACE="$(./11-guix-workstation-get-namespace.sh)"

ip link add macvlan2 link br0 type macvlan mode bridge
ip link set macvlan2 netns "$NAMESPACE"
ip netns exec "$NAMESPACE" ip link set macvlan2 up
ip netns exec "$NAMESPACE" ip addr add 192.168.0.196/24 dev macvlan2
ip netns exec "$NAMESPACE" ping -c 3 192.168.0.145
