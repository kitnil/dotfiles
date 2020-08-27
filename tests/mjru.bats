#!/usr/bin/env -S bats

@test "run router1.intr" {
    run ssh router1.intr -- id
    [ "$status" -eq 0 ]
    [[ "$output" == *"uid=1000(eng)"* ]]
}

@test "run router2.intr" {
    run ssh router2.intr -- id
    [ "$status" -eq 0 ]
    [[ "$output" == *"uid=1000(eng)"* ]]
}

@test "run router4.intr" {
    skip
    run bash --rcfile dot_bash.d/mjru.bash -i -c 'router4.intr -- id'
    [ "$status" -eq 0 ]
    [[ "$output" == *"uid=0(root)"* ]]
}

@test "run mjdev.intr" {
    run bash --rcfile dot_bash.d/mjru.bash -i -c 'mjdev.intr -- id'
    [ "$status" -eq 0 ]
    [[ "$output" == *"uid=0(root)"* ]]
}
