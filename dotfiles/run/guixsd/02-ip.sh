#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

# ip address add 127.0.0.2/8 dev enp34s0
ip address add 192.168.0.144/24 dev enp34s0
ip address add 192.168.0.145/24 dev enp34s0

ip link set br154 up
# ip address add 127.0.0.3/8 dev br154

ip link set br155-vlan155 up
# ip address add 127.0.0.3/8 dev 127.0.0.4/8

ip link set br154.154 up
ip address add 192.168.154.1/24 dev br154.154

ip link set br156 up
ip address add 127.0.0.156/8 dev br156

ip link set br156.156 up
ip address add 192.168.156.1/24 dev br156.156

modprobe netconsole netconsole=@/enp34s0,6666@192.168.0.194/
