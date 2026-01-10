{ config, pkgs, lib, ... }:

with lib;

let
  name = "google-chrome";
  cfg = config.services.google-chrome;
in
{
  options = {
    services.webhook-custom = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Webhook Custom.
        '';
      };
      flake = mkOption {
        type = types.str;
        description = ''
          Flake for system reconfigure.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    services.webhook = {
      enable = true;
      port = 9000;
      openFirewall = true;
      user = "root";
      group = "root";
      hooks = {
        reconfigure = {
          execute-command = pkgs.writeText "webhook.json" (builtins.toJSON {
            id = "reconfigure";
            "execute-command" = pkgs.writeScript "webhook-reconfigure.sh" ''
              PATH=${pkgs.git}/bin:${pkgs.coreutils}/bin:$PATH
              export PATH
              workspace="$(mktemp -d -t "dotfiles.XXXXXXXXXX")"
              cd "$workspace" || exit 1
              git clone --depth 1 https://cgit.wugi.into/wigust/dotfiles .
              nixos-rebuild switch --flake ${flake} -L
            '';
          });
        };
      };
      hooksTemplated = {
        reconfigureTemplate = builtins.toJSON {
          id = "reconfigure-template";
          "execute-command" = "reconfigure";
        };
      };
    };
  };
}
