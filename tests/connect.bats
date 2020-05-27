#!/usr/bin/env -S bats

@test "connect ping web30.intr" {
    run bin/executable_connect ping web30.intr
    [ "$status" -eq 0 ]
    [[ "$output" == *64\ bytes* ]]
}

@test "connect containers web30.intr" {
    run bin/executable_connect containers web30.intr
    [ "$status" -eq 0 ]
    [[ "$output" == *nginx* ]]
}
