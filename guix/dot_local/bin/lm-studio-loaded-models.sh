#!/usr/bin/env bash

curl --fail --silent http://192.168.0.178:1234/api/v0/models | jq --compact-output --raw-output '.data[] | select(.state == "loaded") | { text: .id, tooltip: .max_context_length }'
