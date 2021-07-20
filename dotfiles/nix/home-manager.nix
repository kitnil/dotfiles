{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    nixFlakes
  ];

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      gesturefy
      ublock-origin
    ];
    profiles = {
      default = {
        # This profile not managed by Nix.
        name = "default";
        path = "j56dvo43.default-1520714705340";
        isDefault = true;
        id = 0;
      };
      nix = {
        name = "nix";
        id = 1;
      };
    };
  };
}
