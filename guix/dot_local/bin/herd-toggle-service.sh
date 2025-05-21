#!/usr/bin/env bash

set -o errexit -o pipefail -o xtrace

if [[ $(sudo herd status | awk "/$1/ { print \$1 }") == "-" ]]
then
    echo sudo herd start "$1"
else
    echo sudo herd stop "$1"
fi
