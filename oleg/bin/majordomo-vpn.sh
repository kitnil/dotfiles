#!/bin/sh

# This scripts establish a VPN tunnel to Majordomo and setups NAT rules.

openvpn --config /etc/openvpn/mj-client.conf

# NAT
iptables -t nat -A POSTROUTING -o tap0 -j MASQUERADE
iptables -A FORWARD -i tap0 -o br0 -m state --state RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i br0 -o tap0 -j ACCEPT
