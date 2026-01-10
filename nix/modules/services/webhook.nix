{ config, pkgs, lib, ... }:

with lib;

let
  name = "webhook-custom";
  cfg = config.services.webhook-custom;
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
          execute-command = builtins.toString (pkgs.writeScript "webhook-reconfigure.sh" ''
            #!${pkgs.runtimeShell}
            set -o nounset -o errexit -o pipefail -o xtrace
            PATH=${pkgs.git}/bin:${pkgs.coreutils}/bin:$PATH
            export PATH
            if [[ -e /tmp/webhook-reconfigure.txt ]]
            then
                echo "'/tmp/webhook-reconfigure.txt' file exists, is another reconfigure in progress?"
            fi
            workspace="$(mktemp -d -t "dotfiles.XXXXXXXXXX")"
            trap 'rm -rf "$workspace"; rm -f /tmp/webhook-reconfigure.txt' EXIT
            touch /tmp/webhook-reconfigure.txt
            (
                cd "$workspace" || exit 1
                git clone https://cgit.wugi.info/wigust/dotfiles .
                cd nix || exit 1
                if /run/current-system/sw/bin/nixos-rebuild switch --print-build-logs --flake ${cfg.flake}
                then
                    :
                fi
            )
          '');
        };
      };
      hooksTemplated = {
        reconfigureTemplate = builtins.toJSON {
          id = "reconfigure-template";
          "execute-command" = "reconfigure";
        };
      };
    };
    systemd.services.webhook = {
      stopIfChanged = false;
      restartIfChanged = true;
      serviceConfig = {
        Restart = lib.mkForce "always";
      };
    };
  };
}
