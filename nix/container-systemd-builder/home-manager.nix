{ pkgs, lib, config, ... }:

{
  home.username = "oleg";
  home.homeDirectory = "/home/oleg";
  manual.manpages.enable = false;

  home.packages = with pkgs; [
    autoconf
    automake
    dtach
    git
    gnumake
    guile
    strace
    viddy
  ];

  home.file = {
    ".ssh/known_hosts" = {
      text = ''
        gitlab.corp1.majordomo.ru ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJw9vd+rL+MwVdVKSKW32+k6irAULLUFv5dmRUve2nUW
        gitlab.corp1.majordomo.ru ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIpKca//ukVhXODbccv/mv4oG74h8jyNQmF7ZbWd/qaolkBv0ptb/ocPc47+btv+FQTx3Fj/cPyi83kwf3ow7C8=
        gitlab.intr ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJw9vd+rL+MwVdVKSKW32+k6irAULLUFv5dmRUve2nUW
        guixsd ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDaa8s3t+VdVXSbltQyNkn1rnIg8NoXrH1HoYNgqN9xar9jD2A5zkgpiS0iwW/bDWEx5nuwKoGZpUbJ1zn3IAXMVZEHyV0Toji/CpJghECT9G3ocxBoE3Fqx74sWvbWxVMSSvvUkNZAYOIgN8Ud9tUWw8jr+gy4ZKPQvs23BUBMIKZe9LMta4Y42aIK3ofCMsqZbEOsBIbiq7LoCoaKYCiNXshD3UEN4h1vrulokYt7sXNLl1UIJl0uwI1HiBKHjyfbQXCRUCLwwvi/EcFseAaq4jG8kSYkKu+5KalNP7BlYcui0Hi36RnPf3UjQGbjheDtZs/Xv5apgD1Q9CUXVXKl
        guixsd ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBOnaDeOzwmrcrq1D8slYaeFozXZ0cpqNU0EvGmgnO29aiKkSD1ehbIV4vSxk3IDXz9ClMVPc1bTUTrYhEVHdCks=
       guixsd ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMRy+enQECs2CsjrbSIfnNHExcUwzHFa7KUnhwDIeWOV
      '';
    };
    ".gitconfig" = {
      text = ''
        [user]
          email = go.wigust@gmail.com
          name = Oleg Pykhalov
      '';
    };
  };

  programs.home-manager.enable = true;

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    enableBashIntegration = true;
    defaultCacheTtl = 172800;
    defaultCacheTtlSsh = 172800;
    maxCacheTtl = 172800;
    maxCacheTtlSsh = 172800;
    pinentryFlavor = "tty";
    grabKeyboardAndMouse = false;
    extraConfig = ''
      allow-preset-passphrase
    '';
  };

  programs.ssh = {
    enable = true;
    extraConfig = ''
      Host gitlab.intr
      User git
      IdentityFile /home/oleg/.ssh/id_rsa_gitlab_intr_nopass

      Host gitlab.corp1.majordomo.ru
      User git
      IdentityFile /home/oleg/.ssh/id_rsa_gitlab_intr_nopass

      Host guixsd
      User git
      IdentityFile /home/oleg/.ssh/id_ed25519
    '';
  };

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "23.05";
}
