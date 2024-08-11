#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

exec swaymsg output HEADLESS-1 resolution 1800x950
