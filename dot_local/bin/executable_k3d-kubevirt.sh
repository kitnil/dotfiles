#!/usr/bin/env bash

set -x

k3d cluster create -p "30022:30022@agent:0" --agents 2 --volume /var/run/kubevirt:/var/run/kubevirt:shared cluster1

export KV_RELEASE=v0.55.0
kubectl apply -f https://github.com/kubevirt/kubevirt/releases/download/${KV_RELEASE}/kubevirt-operator.yaml
kubectl apply -f https://github.com/kubevirt/kubevirt/releases/download/${KV_RELEASE}/kubevirt-cr.yaml

# wait until all KubeVirt components are up
kubectl -n kubevirt wait kv kubevirt --for condition=Available

export CDI_RELEASE=v1.52.0
kubectl apply -f https://github.com/kubevirt/containerized-data-importer/releases/download/$CDI_RELEASE/cdi-operator.yaml
kubectl apply -f https://github.com/kubevirt/containerized-data-importer/releases/download/$CDI_RELEASE/cdi-cr.yaml
