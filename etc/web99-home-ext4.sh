#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

lvcreate -L 45G -n web99homeext4 vg0
mkfs.ext4 -L web99homeext4 /dev/vg0/web99homeext4
