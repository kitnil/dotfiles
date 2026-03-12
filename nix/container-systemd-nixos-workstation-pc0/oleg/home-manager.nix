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
      obs-studio-plugins.obs-vkcapture

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

  systemd.user.services.xwayland-satellite = {
    Unit = {
      Description = "Xwayland-Satellite terminal";
      StartLimitBurst = 5;
      StartLimitIntervalSec = 10;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
    Service = {
      Environment = [
        "XDG_RUNTIME_DIR=/mnt/guix/run/user/%U"
        "WAYLAND_DISPLAY=wayland-1"
      ];
      ExecStart = "${pkgs.xwayland-satellite}/bin/xwayland-satellite";
      Type = "simple";
      Restart = "always";
      RestartSec = "2s";
    };
  };
}
