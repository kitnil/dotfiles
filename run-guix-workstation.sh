#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

/home/oleg/.local/share/chezmoi/rerun.sh
ssh 192.168.0.192 <<'EOF'
PATH="/home/oleg/.guix-profile/bin:/gnu/store/3q2x34wg1fff833wwzxnagnv7vbfxb0w-jc-1.25.2/bin:$PATH"
export PATH

guix_workstation_id()
{
    sudo nerdctl -n k8s.io ps --format=json \
        | jq --exit-status --raw-output '. | select (.Image | startswith("harbor.home.wugi.info/library/guix-image-workstation")) | .ID'
}

echo "INFO: Waiting Guix workstation container is running." 1>&2
until guix_workstation_id
do
    sleep 2
done
echo "INFO: Waiting service in Guix workstation are running for 10 seconds." 1>&2
sleep 10
sudo /root/run.py
sleep 1
sudo /root/run.py
EOF
echo "INFO: Waiting Guix workstation container to start for 10 seconds." 1>&2
sleep 10
/home/oleg/.local/share/chezmoi/rerun.2.sh
