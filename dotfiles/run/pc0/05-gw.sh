#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo ip route replace default via 192.168.0.145
