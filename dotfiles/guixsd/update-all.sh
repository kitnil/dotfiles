#!/bin/sh
# Update my packages, system and deploy.

set -e -x

# Update packages specified in my manifest.
guix package --manifest="$HOME/src/dotfiles/guix/manifests/oleg.scm"

# Update packages specified in my manifest on remote hosts.
for host in workstation spb; do
    ssh "$host" -- guix package --manifest=manifest.scm
done

# Reconfigure current and remote hosts.
(
    cd "$HOME/src/dotfiles" || exit
    for target in guixsd deploy; do
        make --always-make "$target"
    done
)
