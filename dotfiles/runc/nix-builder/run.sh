#!/usr/bin/env bash

/gnu/store/3dwzy5gwagj0g838gr4kg77a0s0v1q0s-runc-1.1.12/sbin/runc delete nix-builder
exec /gnu/store/3dwzy5gwagj0g838gr4kg77a0s0v1q0s-runc-1.1.12/sbin/runc run --detach nix-builder
