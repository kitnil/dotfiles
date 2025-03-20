{ pkgs, lib, config, ... }:

{
  home.username = "oleg";
  home.homeDirectory = "/home/oleg";
  manual.manpages.enable = false;

  home.packages = with pkgs; [
    gnumake
    strace
    tmux
    viddy
  ];

  home.file = {
    ".ssh/known_hosts" = {
      text = ''
        gitlab.corp1.majordomo.ru ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJw9vd+rL+MwVdVKSKW32+k6irAULLUFv5dmRUve2nUW
        gitlab.corp1.majordomo.ru ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIpKca//ukVhXODbccv/mv4oG74h8jyNQmF7ZbWd/qaolkBv0ptb/ocPc47+btv+FQTx3Fj/cPyi83kwf3ow7C8=
        gitlab.intr ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJw9vd+rL+MwVdVKSKW32+k6irAULLUFv5dmRUve2nUW
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

  programs.ssh = {
    enable = true;
    extraConfig = ''
      Host gitlab.intr
      User git
      IdentityFile /home/oleg/.ssh/id_rsa_gitlab_intr_nopass

      Host gitlab.corp1.majordomo.ru
      User git
      IdentityFile /home/oleg/.ssh/id_rsa_gitlab_intr_nopass
    '';
  };

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "23.05";
}
