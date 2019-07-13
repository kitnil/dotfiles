#!/bin/sh

set -o errexit
set -o nounset
set -o xtrace
set -o pipefail

guix pull --profile=dotfiles --channels=channels.scm

GUIX_BIN="./dotfiles/bin/guix"
"$GUIX_BIN" describe
"$GUIX_BIN" environment --manifest=guix/manifests/oleg.scm -- sh -c exit
"$GUIX_BIN" system build --load-path=fiore/modules guixsd/config.scm
