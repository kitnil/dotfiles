#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo virsh net-start --network br0
sudo virsh start --domain openwrt
