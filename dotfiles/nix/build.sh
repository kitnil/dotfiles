#!/usr/bin/env bash

for package in $(nix flake show |& awk '/: package/ { print $2 }' | sed 's/├───//; s/://; s,\x1B\[[0-9;]*[a-zA-Z],,g')
do
    printf "@ %s\n" "$package"
    nix build .#${package}
done
