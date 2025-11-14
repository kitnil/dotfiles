{
  nix = {
    gc.automatic = true;
    gc.options = "--delete-older-than 14d";
    settings = {
      trusted-users = [ "root" "eng" "jenkins" ];
    };
  };
}
