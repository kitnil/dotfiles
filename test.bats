#!/usr/bin/env -S bats -t

@test "run bin/executable_jenkins-nix-version" {
    command="bin/executable_jenkins-nix-version hello"
    echo "Invoking: '$command'." >&3; run $command; echo "$output" >&3
    [ "$status" -eq 0 ]
    [[ "$output" == /nix/store/*-hello-* ]]
}
