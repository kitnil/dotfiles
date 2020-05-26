#!/usr/bin/env -S bats

@test "run bin/executable_jenkins-nix-version" {
    run bin/executable_jenkins-nix-version hello
    [ "$status" -eq 0 ]
    [[ "$output" == /nix/store/*-hello-* ]]
}

@test "run bin/executable_camera" {
    run timeout 3 bin/executable_camera --vo=null
    [ "$status" -eq 124 ]
    [[ "$output" == *"640x480 yuv420p"* ]]
}
