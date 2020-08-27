#!/usr/bin/env -S bats

@test "run connect ping web30.intr" {
    run timeout 3 bin/executable_connect ping web30.intr
    [ "$status" -eq 0 ]
    [[ "$output" == *64\ bytes* ]]
}

@test "run connect containers web30.intr" {
    run timeout 3 bin/executable_connect containers web30.intr
    [ "$status" -eq 0 ]
    [[ "$output" == *nginx* ]]
}

@test "run connect ssh web30.intr" {
    run timeout 3 bin/executable_connect ssh web30.intr
    [ "$status" -eq 124 ]
    [[ "$output" == *root@web30* ]]
}

@test "run connect ip-filter --host=web30.intr --describe" {
    run timeout 3 bin/executable_connect ip-filter --host=web30.intr --describe
    [ "$status" -eq 0 ]
    [[ "$output" == *blocked* ]]
}

@test "run connect shell sw1-mr11.intr show interfaces" {
    run timeout 3 bin/executable_connect shell sw1-mr11.intr show interfaces
    [ "$status" -eq 0 ]
    [[ "$output" == *FastEthernet* ]]
}

@test "run connect br1-mr14.intr -- id" {
    run timeout 3 bin/executable_connect br1-mr14.intr -- id
    [ "$status" -eq 0 ]
    [[ "$output" == *"uid=0(root)"* ]]
}

# XXX: Test exit code
@test "run SSHRC_BECOME=yes connect sshrc web30.intr -- id" {
    run env SSHRC_BECOME=yes timeout 3 bin/executable_connect sshrc web30.intr -- id
    [[ "$output" == *"uid=0(root)"* ]]
}

# XXX: Test exit code
@test "run SSHRC_BECOME=yes connect sshrc jenkins.intr -- id" {
    run env SSHRC_BECOME=yes timeout 3 bin/executable_connect sshrc jenkins.intr -- id
    [[ "$output" == *"uid=0(root)"* ]]
}

