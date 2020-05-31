#!/usr/bin/env -S bats

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
