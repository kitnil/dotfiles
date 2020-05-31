#!/usr/bin/env -S bats

@test "run mail inbox for each address" {
    run bash -c 'mail inbox -A | while IFS= read -r address; do [[ "$(mail inbox "$address")" == *EXISTS* ]]; done'
    [ "$status" -eq 0 ]
}
