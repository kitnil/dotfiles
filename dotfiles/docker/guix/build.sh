#!/gnu/store/x3mwr9npmxdmdzgql0gif1ry8xdlfmis-bash-5.1.16/bin/bash

set -o nounset -o errexit -o pipefail -o xtrace

entrypoint()
{
    docker inspect "$IMG" \
        | jq --join-output --raw-output '.[0].Config.Entrypoint | .[0], " ", .[1]'
}

cat > Dockerfile <<EOF
FROM $IMG
COPY rootfs/bin/entrypoint /bin/entrypoint
ENTRYPOINT [ "/bin/entrypoint $(entrypoint)" ]
EOF
