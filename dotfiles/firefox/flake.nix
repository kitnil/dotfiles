{
  description = "Firefox";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
    in {
      devShell.x86_64-linux = with pkgs; mkShell {
        buildInputs = [ nixFlakes ];
      };
      packages.${system} = with pkgs; rec {
        default = firefox-wrapper;
        firefox-wrapper =
          callPackage ({ stdenv, firefox, nix }:
            writeScriptBin "firefox" ''
              #!${runtimeShell} -e
              set -x
              PATH="${stunnel}/bin:${nix}/bin:${gnused}/bin:${git}/bin:${utillinux}/bin:${ncurses}/bin:${coreutils}/bin:$PATH"
              export PATH
              temp_directory="$(mktemp --directory /tmp/firefox.XXXXXXXXXX)"
              mkdir -p "$temp_directory/src"
              cp --recursive ${./.} "$temp_directory/src/flake"
              trap 'chmod -Rf +w "$temp_directory"; rm -rf "$temp_directory"' EXIT
              export temp_directory
              (
                  cd "$temp_directory/src/flake" || exit 1
                  chmod 0644 flake.nix
                  chmod 755 .
                  sed --in-place "s|nonexistentHome|$HOME|;s|nonexistentUser|''${USER:-root}|" flake.nix
                  sed --in-place "s|@javaws@|${adoptopenjdk-icedtea-web}|" mimeTypes.rdf
                  git init
                  git add -A
                  git config user.email "you@example.com"
                  git config user.name "Your Name"
                  git commit -m 'Initial commit.'
                  nix run .#home-manager -- build --flake .#firefox
              )
              profile="$(readlink -f $temp_directory/src/flake/result)"
              export profile

              run_firefox()
              {
                  USER=''${USER:-root} $profile/activate

                  FONTCONFIG_FILE=${fontconfig.out}/etc/fonts/fonts.conf
                  export FONTCONFIG_FILE

                  mkdir -p $HOME/.config
                  echo 'application/x-java-jnlp-file=javaws.desktop;' > $HOME/.config/mimeapps.list

                  ${firefox}/bin/firefox --new-instance --profile "$HOME/.mozilla/firefox/tmp" "$@"
              }
              export -f run_firefox

              run_firefox_without_home()
              {
                  # Make directories empty to preserve user's files
                  mount --types tmpfs none "$HOME"
                  mount --types tmpfs none "/nix/var/nix/profiles/per-user/''${USER:-root}"
                  run_firefox "$@"
              }
              export -f run_firefox_without_home

              if [ -f /.dockerenv ]
              then
                  exec -a "$0" sh -c "set -e; run_firefox $@"
              else
                  exec -a "$0" unshare --mount --map-root-user --fork sh -c "set -e; run_firefox_without_home $@"
              fi
            '') { firefox = firefox; nix = pkgs.nixFlakes; };
      };
      homeConfigurations = {
        firefox = home-manager.lib.homeManagerConfiguration {
          inherit pkgs system;
          homeDirectory = "nonexistentHome";
          username = "nonexistentUser";
          configuration.imports = [ ./home.nix ];
        };
      };
      apps.${system} = {
        inherit (home-manager.apps.${system}) home-manager;
        firefox = {
          type = "app";
          program = "${self.packages.${system}.firefox-wrapper}/bin/firefox";
        };
      };
      defaultApp.${system} = self.apps.${system}.firefox;
    };
}
