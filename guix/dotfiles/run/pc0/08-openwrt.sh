#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

if sudo virsh net-start --network br0
then
    :
fi

until sudo virsh start --domain openwrt
do
    sleep 2
done
