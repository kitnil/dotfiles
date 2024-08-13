#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo iptables -t nat -N REDSOCKS
sudo iptables -t nat -A REDSOCKS -d 192.168.0.0/24 -j RETURN
sudo iptables -t nat -A REDSOCKS -p tcp -j REDIRECT --to-ports 1082
sudo iptables -t nat -A PREROUTING -i br0 -p tcp -j REDSOCKS
