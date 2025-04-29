#!/usr/bin/env bash

curl --fail --silent https://lm-studio.home.wugi.info/api/v0/models | jq --compact-output --raw-output '.data[] | select(.state == "loaded") | { text: .id, tooltip: .max_context_length }'
