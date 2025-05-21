#!/bin/sh

set -o nounset -o errexit -o pipefail -o xtrace

main()
{
    emerge gui-apps/foot
}

main "$@"
