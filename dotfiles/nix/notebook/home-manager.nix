{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    viddy
  ];

  home.file = {
    ".mozilla/native-messaging-hosts/passff.json" = {
      text = builtins.toJSON {
        allowed_extensions = [ "passff@wugi.info" ];
        description = "Host for communicating with zx2c4 pass";
        name = "passff";
        path = "${pkgs.passff-host}/share/passff-host/passff.py";
        type = "stdio";
      };
    };
  };

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
        settings = {
          "browser.startup.homepage" = "about:addons";
          "browser.search.region" = "GB";
          "distribution.searchplugins.defaultLocale" = "en-GB";
          "general.useragent.locale" = "en-GB";
          "browser.search.defaultenginename" = "Google";
        };
      };
    };
  };
}
