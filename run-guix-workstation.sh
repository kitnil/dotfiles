#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

if kubectl -n workstation delete --wait=false pod/workstation
then
    :
fi

check()
{
    if ssh 192.168.0.192 sudo nerdctl -n k8s.io ps | grep --quiet workstation
    then
        return 1
    else
        if kubectl -n workstation delete --force pod/workstation
        then
            :
        fi
    fi
}

until check
do
    sleep 2
done

cd /home/oleg/.local/share/chezmoi/apps/cluster1/workstation || exit 1
flux reconcile kustomization workstation

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

echo "INFO: Waiting Guix workstation container to start for 10 seconds." 1>&2

bar()
{
if sudo /home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg -t get_outputs'
then
    sudo herd restart container-guix-sway-autostart
    exit 0
fi
}
until bar
do
    sleep 2
done
EOF
