#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

modprobe kvmfr static_size_mb=128
chown oleg:kvm /dev/kvmfr0
