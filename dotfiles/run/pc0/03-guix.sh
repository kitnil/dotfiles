#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

sudo mount -t nfs 192.168.0.145:/srv/lib/video /srv/lib/video
sshfs 192.168.0.145:/srv/obs /srv/obs
sshfs 192.168.0.145:/srv/video /srv/video
