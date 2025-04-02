#!/usr/bin/env bash

set -xe

input="$1"

readarray -t tor_bridges <<< "$input"

pass insert --force --multiline tor/bridge/1 < <(echo "${tor_bridges[0]}")
pass insert --force --multiline tor/bridge/2 < <(echo "${tor_bridges[1]}")
