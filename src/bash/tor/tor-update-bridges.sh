#!/usr/bin/env bash

# Example:
#
# ./tor-update-bridges.sh 'obfs4 ooo.ooo.ooo.ooo:oooo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx cert=yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy iat-mode=0
# obfs4 ooo.ooo.ooo.ooo:oooo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx cert=yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy iat-mode=0'

set -o nounset -o errexit -o pipefail -o xtrace

/home/oleg/.local/share/chezmoi/src/bash/tor/tor-bridges-password-store.sh "$1"
/home/oleg/.local/share/chezmoi/apps/cluster1/tor-controller-instance/tor-configuration.sh
kubectl --namespace tor-controller-instance delete pod --all
