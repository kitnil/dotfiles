#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo ip r add 78.108.80.0/24 via 192.168.0.145
sudo ip route add 172.16.103.0/24 via 192.168.0.145
