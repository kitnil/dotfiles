#!/bin/sh

set -o errexit
set -o xtrace
set -o pipefail

# This scripts establish a VPN tunnel to Majordomo and setups NAT rules.

tap="$1"

openvpn --config /etc/openvpn/mj-client.conf

# NAT
iptables -t nat -A POSTROUTING -o "$tap" -j MASQUERADE
iptables -A FORWARD -i "$tap" -o br0 -m state --state RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i br0 -o "$tap" -j ACCEPT

# HMS billing
ip r add 78.108.80.178/32 via 172.16.100.3
ip r add 78.108.80.171/32 via 172.16.100.3
