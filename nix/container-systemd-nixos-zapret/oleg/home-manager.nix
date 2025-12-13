{ pkgs, lib, config, customLib, ... }:

let
  inherit (customLib) firefoxBaseProfile;
in
{
  programs.chromium = {
    enable = true;
    commandLineArgs = [ "--ozone-platform=wayland" ];
  };
  programs.firefox = {
    profiles = {
      socialstream = (firefoxBaseProfile { ech = false; }) // {
        name = "socialstream";
        id = 42187;
        isDefault = false;
      };
    };
  };
  home.stateVersion = "23.05";
}
