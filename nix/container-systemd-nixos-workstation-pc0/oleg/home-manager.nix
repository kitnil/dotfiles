{ pkgs, lib, config, customLib, ... }:

let
  inherit (customLib) firefoxBaseProfile;
in
{
  imports = [
    ./../private.nix
  ];
  home.packages = [
    pkgs.firejail
    pkgs.fuzzel
  ];
  programs.chromium = {
    enable = true;
    commandLineArgs = [ "--ozone-platform=wayland" ];
  };
  programs.obs-studio = {
    enable = true;
    plugins = with pkgs; [
      espanso-wayland

      obs-studio-plugins.obs-multi-rtmp
      obs-studio-plugins.obs-pipewire-audio-capture

      socialstream

      yt-title-updater-python
      streamtitle

      steam

      vulkan-tools

      jetbrains.pycharm-oss

      kubernetes-helm
    ];
  };
  programs.firefox = {
    profiles = {
      stream-manager = (firefoxBaseProfile { ech = false; }) // {
        name = "stream-manager";
        id = 18243;
        isDefault = false;
      };
    };
  };
  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
    size = 32;
  };
}
