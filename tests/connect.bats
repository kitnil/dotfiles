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

@test "connect ssh web30.intr" {
    run timeout 3 bin/executable_connect ssh web30.intr
    [ "$status" -eq 124 ]
    [[ "$output" == *root@web30* ]]
}
