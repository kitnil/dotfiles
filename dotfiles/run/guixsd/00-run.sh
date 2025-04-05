#!/usr/bin/env bash

set -e

./01-luks.sh
./02-dri.sh
./04-kubelet.sh
./08-openwrt.sh
