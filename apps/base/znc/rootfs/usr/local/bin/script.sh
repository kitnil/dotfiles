#!/bin/sh

set -o nounset -o errexit -o pipefail -o xtrace

if [[ -e /znc-data/configs/znc.conf ]]
then
    :
else
    znc --makeconf
fi
