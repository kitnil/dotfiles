#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

gateway="$(ip -json route | jq --raw-output '.[] | select(."dst" == "default") | .gateway')"

ip route add 192.168.0.192/32 via "$gateway"
