#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo ip link add macvlan1 link eth0 type macvlan mode bridge
sudo ip link set macvlan1 netns "$NAMESPACE"
