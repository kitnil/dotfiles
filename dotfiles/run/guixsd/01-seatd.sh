#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

exec sudo seatd
