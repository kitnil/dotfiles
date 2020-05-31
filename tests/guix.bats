#!/usr/bin/env -S bats

@test "create cache directory" {
    mkdir -p cache
}

@test "run guix pull" {
    run guix pull                               \
            --verbosity=0                       \
            --profile=cache/.guix-profile       \
            --channels=dotfiles/channels.scm
    [ "$status" -eq 0 ]
}

@test "build guixsd system" {
    run cache/.guix-profile/bin/guix system build \
            --load-path=dotfiles/fiore/modules dotfiles/guixsd/guixsd.scm
    [ "$status" -eq 0 ]
}

@test "build spb system" {
    run cache/.guix-profile/bin/guix system build \
            --load-path=dotfiles/fiore/modules dotfiles/guixsd/spb.scm
    [ "$status" -eq 0 ]
}

@test "build workstation-guixsd system" {
    run cache/.guix-profile/bin/guix system build \
            --load-path=dotfiles/fiore/modules dotfiles/guixsd/workstation-guixsd.scm
    [ "$status" -eq 0 ]
}

@test "build guixsd manifest" {
    run cache/.guix-profile/bin/guix environment \
            --manifest=dotfiles/manifests/guixsd.scm -- sh -c exit
    [ "$status" -eq 0 ]
}

@test "build spb manifest" {
    run cache/.guix-profile/bin/guix environment \
            --manifest=dotfiles/manifests/spb.scm -- sh -c exit
    [ "$status" -eq 0 ]
}

@test "build workstation-guixsd manifest" {
    run cache/.guix-profile/bin/guix environment \
            --manifest=dotfiles/manifests/workstation-guixsd.scm -- sh -c exit
    [ "$status" -eq 0 ]
}

