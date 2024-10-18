#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

rsync --ignore-existing --archive /mnt/home/oleg/.gnupg/ /home/oleg/.gnupg/
