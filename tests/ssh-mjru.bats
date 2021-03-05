@test "invoke logout command over ssh on sw2-dh507.intr" {
    skip
    run timeout 30 sshpass -p"$(pass show majordomo/public/ssh/router)" ssh -F test-tmp/config -o ControlPath=none sw2-dh507.intr -- logout
    [ "$status" -eq 0 ]
}

@test "invoke logout command over ssh on sw2-dh508.intr" {
    run timeout 30 sshpass -p"$(pass show majordomo/public/ssh/router)" ssh -F test-tmp/config -o ControlPath=none sw2-dh508.intr -- logout
    [ "$status" -eq 0 ]
}

@test "invoke id command over ssh on br1-mr14.intr" {
    run timeout 30 sshpass -p"$(pass show majordomo/public/ssh/router)" ssh -F test-tmp/config -o ControlPath=none br1-mr14.intr -- id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=0* ]]
}

@test "invoke id command over ssh on sr1-dh507-508.intr" {
    run timeout 30 sshpass -p"$(pass show majordomo/public/ssh/router)" ssh -F test-tmp/config -o ControlPath=none sr1-dh507-508.intr -- id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=0* ]]
}

@test "invoke id command over ssh on web30" {
    run timeout 30 ssh -F test-tmp/config -o ControlPath=none web30.intr id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=1000* ]]
}

@test "invoke id command over ssh on mr.backup.majordomo.ru" {
    skip
    run timeout 30 ssh -F test-tmp/config -o ControlPath=none mr.backup.majordomo.ru id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=1000* ]]
}

@test "invoke id command over ssh on router4.intr" {
    run timeout 30 sshpass -p"$(pass show majordomo/public/router4/root)" ssh -F test-tmp/config -o ControlPath=none router4.intr id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=* ]]
}

@test "invoke id command over ssh on galera-backup.intr" {
    run timeout 30 ssh -F test-tmp/config -o ControlPath=none galera-backup.intr id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=1000* ]]
}

@test "invoke id command over ssh on malscan.intr" {
    run timeout 30 ssh -F test-tmp/config -o ControlPath=none malscan.intr id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=* ]]
}

@test "invoke id command over ssh on vman.intr" {
    run timeout 30 ssh -F test-tmp/config -o ControlPath=none vman.intr id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=* ]]
}

@test "invoke id command over ssh on mjdev.intr" {
    run timeout 30 sshpass -p"$(pass show majordomo/public/mjdev.intr/root)" ssh -F test-tmp/config -o ControlPath=none mjdev.intr id
    [ "$status" -eq 0 ]
    [[ "$output" == *uid=* ]]
}
