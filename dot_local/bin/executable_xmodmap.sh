#!/usr/bin/env bash

for x in {1..10}
do
    printf "[ %s/10 ] Apply ~/.Xmodmap.\n" "$x"
    while [[ $(xmodmap -pke) == *"keycode 134 = Super_R NoSymbol Super_R"* ]]
    do
        xmodmap ~/.Xmodmap
    done
    sleep 1
done
