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
