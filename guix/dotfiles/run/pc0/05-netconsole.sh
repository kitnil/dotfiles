#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

socat UDP-LISTEN:6666,fork - | tee -a ~/netconsole.txt
