#!/usr/bin/env -S bats

@test "run router4.intr" {
    skip
    run bash --rcfile dot_bash.d/majordomo.bash -i -c 'router4.intr -- id'
    [ "$status" -eq 0 ]
    [[ "$output" == *"uid=0(root)"* ]]
}

@test "run mjdev.intr" {
    run bash --rcfile dot_bash.d/majordomo.bash -i -c 'mjdev.intr -- id'
    [ "$status" -eq 0 ]
    [[ "$output" == *"uid=0(root)"* ]]
}
