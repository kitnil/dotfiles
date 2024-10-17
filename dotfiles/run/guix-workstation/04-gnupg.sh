#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

cp --no-clobber --recursive /mnt/home/oleg/.gnupg /home/oleg/.gnupg
