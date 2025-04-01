#!/bin/bash

set -o nounset -o errexit -o pipefail -o xtrace

kubectl config set-cluster cfc \
        --server=https://kubernetes-ingress-nginx-controller.ingress-nginx:6443 \
        --certificate-authority=/var/run/secrets/kubernetes.io/serviceaccount/ca.crt
kubectl config set-context cfc --cluster=cfc
kubectl config set-credentials user \
        --token="$(cat /var/run/secrets/kubernetes.io/serviceaccount/token)"
kubectl config set-context cfc --user=user
kubectl config use-context cfc
cat "${HOME}/.kube/config"
