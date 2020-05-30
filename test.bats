#!/usr/bin/env -S bats

@test "find non executables in bin directory" {
    run find bin -not -executable
    [ "$status" -eq 0 ]
    [ -z "$output" ]
}

@test "run bin/executable_jenkins-nix-version" {
    run bin/executable_jenkins-nix-version hello
    [ "$status" -eq 0 ]
    [[ "$output" == /nix/store/*-hello-* ]]
}

@test "run bin/executable_camera" {
    run timeout 3 bin/executable_camera --vo=null
    [ "$status" -eq 124 ]
    [[ "$output" == *"640x480 yuv420p"* ]]
}

@test "imap yandex" {
    run curl -u"houdinihar:$(pass show email/yandex.ru/houdinihar)" imaps://imap.yandex.ru:993
    [ "$status" -eq 0 ]
    [[ "$output" == *INBOX* ]]
}

@test "imap mail.ru" {
    run curl -u"asfjsdf@mail.ru:$(pass show mail.ru/asfjsdf)" imaps://imap.mail.ru:993
    [ "$status" -eq 0 ]
    [[ "$output" == *INBOX* ]]
}
