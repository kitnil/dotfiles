#!/usr/bin/env -S nix eval --json --file

let
  inherit (builtins) getFlake toPath toJSON;
  flake = getFlake git+https://gitlab.intr/net/dns-intr;
  inherit (flake.lib) zone;
  inherit (flake.inputs.nixpkgs.lib)
    attrNames fold mapAttrs' nameValuePair filterAttrs hasPrefix;
  inherit (flake.inputs.dns.lib) evalZone;
  zone-name = "intr";
in
attrNames
  (filterAttrs
    (name: value: value != [])
    (mapAttrs'
      (name: value: nameValuePair (name + "." + zone-name) value.A)
      ((evalZone zone-name zone).subdomains)))
