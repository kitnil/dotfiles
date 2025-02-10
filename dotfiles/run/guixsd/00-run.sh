#!/usr/bin/env bash

set -e

./01-luks.sh
./07-virsh.sh
./04-kubelet.sh
./08-openwrt.sh
