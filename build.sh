#!/bin/sh

set -o errexit
set -o nounset
set -o xtrace
set -o pipefail

guix pull --verbosity=0 --profile=.guix-profile --channels=channels.scm

GUIX_BIN=".guix-profile/bin/guix"
"$GUIX_BIN" describe
for system in config spb; do
    "$GUIX_BIN" system build --verbosity=0 \
        --load-path=fiore/modules "guixsd/$system.scm"
done
for manifest in oleg maintenance; do
    "$GUIX_BIN" environment --verbosity=0 \
        --manifest="guix/manifests/$manifest.scm" -- sh -c exit
done
