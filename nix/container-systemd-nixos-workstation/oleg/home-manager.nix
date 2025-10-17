{ pkgs, lib, config, ... }:

{
  programs.chromium = {
    enable = true;
    commandLineArgs = [ "--ozone-platform=wayland" ];
  };
  programs.obs-studio = {
    enable = true;
  };
}
