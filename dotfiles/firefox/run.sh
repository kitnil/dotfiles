#!/bin/bash

set -ex

~/.nix-profile/bin/nix-build --option trusted-public-keys "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.nixos.intr:6VD7bofl5zZFTEwsIDsUypprsgl7r9I+7OGY4WsubFA=" --substituters "https://cache.nixos.org/ https://cache.nixos.intr/" --no-out-link --expr 'with import (builtins.fetchTarball {url = "https://github.com/nixos/nixpkgs/archive/83fff69fca0d1f7775fbcd4f8ec8acceb1a5bebb.tar.gz";}) { config = {allowBroken = true; allowUnfree = true; allowUnsupportedSystem = true; firefox.icedtea = true; permittedInsecurePackages = ["autotrace-0.31.1" "batik-1.6" "firefox-52.9.0esr" "firefox-esr-unwrapped-52.9.0esr"];};}; firefox-esr-52'
