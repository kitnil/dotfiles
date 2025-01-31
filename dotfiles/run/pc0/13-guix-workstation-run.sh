#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

PATH="/home/oleg/.guix-profile/bin:/gnu/store/3q2x34wg1fff833wwzxnagnv7vbfxb0w-jc-1.25.2/bin:$PATH"
export PATH

guix_workstation_id()
{
    nerdctl -n k8s.io ps --format=json \
        | jq --exit-status --raw-output '. | select (.Image | startswith("harbor.home.wugi.info/library/guix-image-workstation")) | .ID'
}

until guix_workstation_id
do
    sleep 2
done
container_id="$(guix_workstation_id)"

/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/10-avahi-namespace.sh
/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/09-firefox-twitch-namespace.sh

nerdctl -n k8s.io exec "$container_id" /run/current-system/profile/bin/bash -lc '
export PATH=/home/oleg/.guix-home/profile/bin:$PATH
if /home/oleg/bin/manual-scripts-root-02-net.sh
then
    :
else
    :
fi

/home/oleg/bin/manual-scripts-root-03-firefox-twitch-namespace.sh
'
