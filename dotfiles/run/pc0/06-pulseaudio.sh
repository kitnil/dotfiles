#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

pactl load-module module-native-protocol-tcp
