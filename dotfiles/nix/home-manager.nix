{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    nixFlakes
  ];

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      ublock-origin
    ];
    profiles = {
      default = {
        name = "nix";
      };
    };
  };
}
