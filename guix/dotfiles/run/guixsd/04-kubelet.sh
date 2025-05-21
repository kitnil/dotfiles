#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

if [[ -e /run/setuid-programs/mount.nfs ]]
then
    sudo mv /run/setuid-programs/mount.nfs /run/setuid-programs/mount.nfs.1
fi

sudo rm /var/lib/kubelet/.maintenance
sudo herd restart kubelet

mapfile -t pods < <(kubectl get -o name --namespace=openebs pods)
for pod in "${pods[@]}"
do
    kubectl wait --namespace=openebs --for=jsonpath='{.status.phase}'=Running --timeout=3600s "$pod"
done

echo sudo mv /run/setuid-programs/mount.nfs.1 /run/setuid-programs/mount.nfs
