#!/usr/bin/env bash

set -o errexit -o pipefail

/gnu/store/pi23ha50207w3z9yz0n2x83x7ars36xm-bind-9.19.24-utils/bin/dig www.wugi.info @10.8.255.254 | /gnu/store/8wlmbjlsxngkrz17d62jqr02kjw7jy0c-jc-1.25.4/bin/jc --dig | jq --raw-output '.[] | .answer[] | .data'
