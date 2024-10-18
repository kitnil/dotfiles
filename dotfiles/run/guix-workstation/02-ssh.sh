#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

cp -a /mnt/home/oleg/.ssh /home/oleg/
chown -R oleg: /home/oleg/.ssh
