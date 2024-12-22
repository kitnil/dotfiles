#!/bin/sh

set -o nounset -o errexit -o pipefail -o xtrace

main()
{
    echo "main"
}

main "$@"
