{ pkgs, lib, config, customLib, ... }:

let
  inherit (customLib) firefoxBaseProfile;
  path-of-building-data-json = with pkgs;
    let
      lua-json = fetchFromGitHub {
        owner = "rxi";
        repo = "json.lua";
        rev = "v0.1.2";
        hash = "sha256-JSKMxF5NSHW3QaELFPWm1sx7kHmOXEPsUkM3i/px7Gk=";
      };
      poeJson = writeScript "pob-json.lua" ''
        json = require('json')
        require('HeadlessWrapper')
        print(json.encode(data.itemBases))
      '';
    in
    pkgs.writeScriptBin "path-of-building-data-json" ''
      #!${pkgs.runtimeShell}
      export LUA_PATH=';;;${lua-json}/?.lua;${pkgs.path-of-building.data}/?.lua;${pkgs.luajit}/share/lua/5.1/?.lua;${pkgs.luajit.pkgs.lua-curl}/share/lua/5.1/?.lua;'
      export LUA_CPATH=';;;${pkgs.luajit.pkgs.lua-curl}/lib/lua/5.1/?.so;${pkgs.luajit.pkgs.luautf8}/lib/lua/5.1/?.so'
      cd ${pkgs.path-of-building.data}/src
      ${pkgs.luajit}/bin/luajit ${poeJson}
    '';
in
{
  programs.chromium = {
    enable = true;
    commandLineArgs = [ "--ozone-platform=wayland" ];
  };
  programs.obs-studio = {
    enable = true;
    plugins = with pkgs; [
      obs-studio-plugins.obs-multi-rtmp
      obs-studio-plugins.obs-ndi
      obs-studio-plugins.obs-pipewire-audio-capture

      path-of-building
      path-of-building-data-json
    ];
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
}
