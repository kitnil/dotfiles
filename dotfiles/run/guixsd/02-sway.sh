#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

sudo chmod a+w /run/seatd.sock

sudo mkdir -p /run/user/1000
sudo chown oleg: /run/user/1000

WLR_BACKENDS=headless
export WLR_BACKENDS

exec sway
