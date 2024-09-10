#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

# https://www.gstatic.com/ipranges/goog.json
mapfile -t networks < <(jq --raw-output '.prefixes[] | select(.ipv4Prefix) | .ipv4Prefix' /home/oleg/.local/share/chezmoi/dotfiles/run/guixsd/goog.json)

for network in "${networks[@]}"
do
    sudo iptables -t nat -A PREROUTING -p tcp --destination "$network" -j REDSOCKS
done
