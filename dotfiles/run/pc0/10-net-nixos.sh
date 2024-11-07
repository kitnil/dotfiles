#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

NAMESPACE="$(/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/11-nerdctl-namespace.sh)"

ip link add macvlan3 link eth0 type macvlan mode bridge
ip link set macvlan3 netns "$NAMESPACE"
ip netns exec "$NAMESPACE" ip link set macvlan3 up
ip netns exec "$NAMESPACE" ip addr add 192.168.0.195/24 dev macvlan3
ip netns exec "$NAMESPACE" ping -c 3 192.168.0.145
