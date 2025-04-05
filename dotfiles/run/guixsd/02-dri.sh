#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo rm /dev/dri/renderD129
sudo rm /dev/dri/card1

sudo ln -s /dev/dri/renderD128 /dev/dri/renderD129
