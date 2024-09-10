#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo iptables -t nat -N REDSOCKS
sudo iptables -t nat -A REDSOCKS -d 192.168.0.0/24 -j RETURN
sudo iptables -t nat -A REDSOCKS -p tcp -j REDIRECT --to-ports 1082

sudo ipset -exist create byedpi hash:ip hashsize 1024 maxelem 655360
sudo iptables -t nat -A PREROUTING -p tcp -m set --match-set byedpi dst -j REDSOCKS
