#!/bin/sh

set -o errexit
set -o xtrace
set -o pipefail

# This scripts establish a VPN tunnel to Majordomo and setups NAT rules.

# username pyhalov
# password ***REMOVED***

tap="$1"

openvpn --config /etc/openvpn/mj-client.conf

# NAT
iptables -t nat -A POSTROUTING -o "$tap" -j MASQUERADE
iptables -A FORWARD -i "$tap" -o br0 -m state --state RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i br0 -o "$tap" -j ACCEPT
