#!/usr/bin/env -S bats

@test "find non executables in bin directory" {
    run find bin -not -executable
    [ "$status" -eq 0 ]
    [ -z "$output" ]
}

@test "run jenkins-nix-version" {
    run bin/executable_jenkins-nix-version hello
    [ "$status" -eq 0 ]
    [[ "$output" == /nix/store/*-hello-* ]]
}
