#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

# TODO: Do not use grep, only jq instead.
node_not_ready()
{
    kubectl get node -o json kube3 2>/dev/null | gron | grep -F 'node.cilium.io/agent-not-ready'
}

start_node()
{
    echo "INFO: Waiting Guix host is running." 1>&2
    until timeout 2 ssh 192.168.0.192 id
    do
        sleep 2
    done

    get_uptime_total_seconds()
    {
        ssh 192.168.0.192 uptime \
            | jc --uptime \
            | jq --raw-output .uptime_total_seconds
    }

    if [[ "$(get_uptime_total_seconds)" -lt 3600 ]]
    then
        ssh 192.168.0.192 sudo chvt 2
        ssh 192.168.0.192 sudo /home/oleg/.local/share/chezmoi/dotfiles/run/pc0/00-run.sh &
        /home/oleg/.local/share/chezmoi/run-guix-workstation.sh
    fi
}

while :
do
    echo "INFO: Waiting Kubernetes node is not available." 1>&2
    if node_not_ready
    then
        start_node
        sleep 2
    else
        sleep 2
    fi
done
