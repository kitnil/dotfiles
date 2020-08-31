#!/usr/bin/env -S bats

@test "compile jenkins service" {
    run sh -c "(set -ex; cd guix; /run/current-system/profile/bin/guile -L . <<< ',m(wigust services jenkins)')"
    echo $output >&3
    [ "$status" -eq 0 ]
}
