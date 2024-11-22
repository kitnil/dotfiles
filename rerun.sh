#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

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
