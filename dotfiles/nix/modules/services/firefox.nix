{ config, lib, pkgs, ... }:

with lib;

{
  config =
    let
      profiles = attrNames config.programs.firefox.profiles;
      firefox-profile = profile:
        {
          Unit = {
            Description = "Firefox web browser";
          };
          Service = {
            ExecStart =
              let
                script = pkgs.writeScript "firefox.sh" ''
                    #!${pkgs.runtimeShell}

                    XDG_RUNTIME_DIR=/mnt/guix/run/user/1000
                    export XDG_RUNTIME_DIR

                    WAYLAND_DISPLAY=wayland-1
                    export WAYLAND_DISPLAY

                    exec -a firefox ${pkgs.firefox}/bin/firefox -P "$@"
                  '';
              in
                "${script} %i";
            Type = "simple";
          };
        };
    in
      {
        systemd.user.services =
          listToAttrs
            (map
              (profile:
                nameValuePair
                  "firefox@${profile}"
                  (firefox-profile profile))
              profiles);
      };
}
