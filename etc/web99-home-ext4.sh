#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

lvcreate -L 45G -n webext4 vg0
mkfs.ext4 -L webext4 /dev/vg0/webext4
