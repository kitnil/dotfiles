{ pkgs, lib, config, ... }:

{
  programs.chromium = {
    enable = true;
    commandLineArgs = [ "--ozone-platform=wayland" ];
  };
  programs.obs-studio = {
    enable = true;
    plugins = with pkgs; [
      obs-studio-plugins.looking-glass-obs
      obs-studio-plugins.obs-multi-rtmp
      obs-studio-plugins.obs-ndi
      obs-studio-plugins.obs-pipewire-audio-capture
    ];
  };
}
