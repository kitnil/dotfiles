#!/usr/bin/env -S bats

@test "create cache directory" {
    mkdir -p test-tmp
}

@test "run guix pull" {
    run guix pull                               \
            --verbosity=0                       \
            --profile=test-tmp/.guix-profile
    [ "$status" -eq 0 ]
}

@test "build guixsd system" {
    run test-tmp/.guix-profile/bin/guix system build \
            --load-path=dotfiles/guixsd/modules dotfiles/guixsd/guixsd.scm
    [ "$status" -eq 0 ]
}

@test "build spb system" {
    run test-tmp/.guix-profile/bin/guix system build \
            --load-path=dotfiles/guixsd/modules dotfiles/guixsd/spb.scm
    [ "$status" -eq 0 ]
}

@test "build ws1.wugi.info system" {
    run test-tmp/.guix-profile/bin/guix system build \
            --load-path=dotfiles/guixsd/modules dotfiles/guixsd/ws1.wugi.info.scm
    [ "$status" -eq 0 ]
}

@test "build guixsd manifest" {
    run test-tmp/.guix-profile/bin/guix environment \
            --load-path=dotfiles/guixsd/modules \
            --manifest=dotfiles/manifests/guixsd.scm -- sh -c exit
    [ "$status" -eq 0 ]
}

@test "build spb manifest" {
    run test-tmp/.guix-profile/bin/guix environment \
            --load-path=dotfiles/guixsd/modules \
            --manifest=dotfiles/manifests/spb.scm -- sh -c exit
    [ "$status" -eq 0 ]
}

@test "build ws1.wugi.info manifest" {
    run test-tmp/.guix-profile/bin/guix environment \
        --load-path=dotfiles/guixsd/modules \
        --manifest=dotfiles/manifests/ws1.wugi.info.scm -- sh -c exit
    [ "$status" -eq 0 ]
}
