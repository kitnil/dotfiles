#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

jq_expression='
.items[] | select(.spec.nodeName == "kube1") | select(.metadata.name | startswith("piraeus-op-ns")) | .metadata.name
'

get_piraeus_op_ns()
{
    kubectl -n piraeus get pod -o json \
        | jq --raw-output "$jq_expression"
}

commands=(
    "drbdadm down pvc-f75da80a-2b8f-4ac4-b767-9d04a7c7dac8"
    "drbdadm up pvc-f75da80a-2b8f-4ac4-b767-9d04a7c7dac8"
    "drbdadm primary pvc-f75da80a-2b8f-4ac4-b767-9d04a7c7dac8"
)

piraeus_op_ns="$(get_piraeus_op_ns)"
for instruction in "${commands[@]}"
do
    eval "kubectl --namespace=piraeus exec ${piraeus_op_ns} -- ${instruction}"
done
