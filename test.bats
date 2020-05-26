#!/usr/bin/env -S bats

@test "run bin/executable_jenkins-nix-version" {
    run bin/executable_jenkins-nix-version hello
    [ "$status" -eq 0 ]
    [[ "$output" == /nix/store/*-hello-* ]]
}
