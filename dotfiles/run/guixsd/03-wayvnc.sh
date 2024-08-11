#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

WAYLAND_DISPLAY=wayland-1
export WAYLAND_DISPLAY

exec wayvnc
