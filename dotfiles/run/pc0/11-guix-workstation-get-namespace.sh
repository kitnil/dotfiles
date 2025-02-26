#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

PATH="/home/oleg/.guix-profile/bin:/gnu/store/3q2x34wg1fff833wwzxnagnv7vbfxb0w-jc-1.25.2/bin:$PATH"
export PATH

container_id="$(nerdctl -n k8s.io ps --format=json | jq --raw-output '. | select (.Names | startswith("k8s://workstation/workstation-kube3/guix")) | .ID')"
process_id="$(nerdctl -n k8s.io top "$container_id" | jc --ps | jq --raw-output '.[0].pid')"
ip_address="$(nsenter -t "$process_id" -n ip -json a | jq --raw-output '.[] | select(.ifname == "eth0") | .addr_info[] | select(.family == "inet") | .local')"
mapfile -t network_namespaces < <(ip -json -all netns | jq --raw-output '.[].name')
for network_namespace in "${network_namespaces[@]}"
do
    if [[ $ip_address == $(ip netns exec "$network_namespace" ip -json a | jq --raw-output '.[] | select(.ifname == "eth0") | .addr_info[] | select(.family == "inet") | .local') ]]
    then
        echo "$network_namespace"
        exit 0
    fi
done
