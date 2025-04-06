#!/usr/bin/env bash

set -e

./01-luks.sh
sudo ./02-ip.sh
./04-kubelet.sh
./08-openwrt.sh
