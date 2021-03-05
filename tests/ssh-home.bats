@test "invoke id command over ssh on spb" {
    run timeout 10 ssh -F test-tmp/config -o ControlPath=none -J zabbix.wugi.info spb id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=1000* ]]
}

@test "invoke id command over ssh on oracle" {
    run timeout 10 ssh -F test-tmp/config -o ControlPath=none oracle id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=1000* ]]
}

@test "invoke id command over ssh on majordomo.intr" {
    run timeout 10 ssh -F test-tmp/config -o ControlPath=none -J zabbix.wugi.info majordomo.intr id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=1000* ]]
}

@test "invoke id command over ssh on workstation.intr" {
    run timeout 10 ssh -F test-tmp/config -o ControlPath=none ws1.wugi.info id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=1000* ]]
}

@test "invoke id command over ssh on nixos.intr" {
    run timeout 10 ssh -F test-tmp/config -o ControlPath=none nixos.intr id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=1000* ]]
}

@test "invoke id command over ssh on guix.intr" {
    skip
    run timeout 10 ssh -F test-tmp/config -o ControlPath=none guix.intr id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=1000* ]]
}
