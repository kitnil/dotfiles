#!/bin/sh

# See <https://lists.gnu.org/archive/html/guix-devel/2018-03/msg00365.html>.

env GUILE_LOAD_PATH=$HOME/dotfiles:$GUILE_LOAD_PATH\
    ./pre-inst-env guix weather\
    --substitute-urls='https://berlin.guixsd.org https://hydra.gnu.org'\
    -m ~/.local/share/chezmoi/dotfiles/fiore/manifests/guix-collection-manifest.scm\
    --system=x86_64-linux
