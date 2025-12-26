# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ lib, ... }:

{
  console.enable = true;
  systemd.services."getty@tty1" = {
    enable = false;
  };
  systemd.services."autovt@tty1" = {
    enable = false;
  };
  systemd.services."getty@tty11" = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
  };

  services.seatd = {
    enable = true;
    user = "oleg";
    group = "users";
  };

  virtualisation.docker.enable = lib.mkForce false;

  networking.firewall.enable = lib.mkForce true;
  services.zapret = {
    enable = true;
    params = [
      "--debug=1"

      # youtube
      "--dpi-desync=multidisorder"
      "--dpi-desync-split-pos=midsld"
      "--dpi-desync-split-pos=host+1"
    ];
    whitelist = [
      "ggpht.com"
      "googlevideo.com"
      "jnn-pa.googleapis.com"
      "stable.dl2.discordapp.net"
      "wide-youtube.l.google.com"
      "youtu.be"
      "youtube.com"
      "youtubeembeddedplayer.googleapis.com"
      "youtubei.googleapis.com"
      "youtubekids.com"
      "youtube-nocookie.com"
      "youtube-ui.l.google.com"
      "yt3.ggpht.com"
      "yt3.googleusercontent.com"
      "yt4.ggpht.com"
      "ytimg.com"
      "ytimg.l.google.com"
      "yt-video-upload.l.google.com"

      "discord.com"
      "discordapp.com"

      "7tv.app"
    ];
  };

  services.bird = {
    enable = true;
    config = builtins.readFile ./bird.conf;
    checkConfig = false;
  };
  networking.firewall.allowedTCPPorts = [ 179 ];
}
