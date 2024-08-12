#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

sudo chmod a+w /run/seatd.sock

sudo mkdir -p /run/user/1000
sudo chown oleg: /run/user/1000

# WLR_LIBINPUT_NO_DEVICES=1
# export WLR_LIBINPUT_NO_DEVICES

# WLR_DRM_DEVICES=/dev/dri/card0
# export WLR_DRM_DEVICES

export DESKTOP_SESSION="sway"
export XDG_CURRENT_DESKTOP="sway"
export XDG_SESSION_DESKTOP="sway"
export XDG_SESSION_TYPE="wayland"
export WLR_BACKENDS="headless,libinput"

exec sway
