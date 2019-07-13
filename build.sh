#!/bin/sh

set -o errexit
set -o nounset
set -o xtrace
set -o pipefail

guix pull --profile=.guix-profile --channels=channels.scm

GUIX_BIN=".guix-profile/bin/guix"
"$GUIX_BIN" describe
for manifest in oleg maintenance; do
    "$GUIX_BIN" environment --manifest="guix/manifests/$manifest.scm" -- sh -c exit
done
"$GUIX_BIN" system build --load-path=fiore/modules guixsd/config.scm
