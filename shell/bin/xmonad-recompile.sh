#!/bin/sh
exec guix environment --pure --ad-hoc gcc-toolchain ghc@7 xmonad ghc-xmonad-contrib xmobar -- xmonad --recompile
