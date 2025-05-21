#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

entrypoint()
{
    sudo nerdctl -n k8s.io inspect "$IMG" \
        | jq --join-output --raw-output '.[0].Config.Entrypoint | .[0], ",", .[1]'
}

cat > Dockerfile <<EOF
FROM $IMG
COPY rootfs/bin/entrypoint /bin/entrypoint
ENTRYPOINT [ /bin/entrypoint, $(entrypoint) ]
EOF
