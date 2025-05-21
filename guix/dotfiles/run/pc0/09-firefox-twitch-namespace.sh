#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

NAMESPACE="$(/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/11-guix-workstation-get-namespace.sh)"

ip link add macvlan1 link eth0 type macvlan mode bridge
ip link set macvlan1 netns "$NAMESPACE"
