{ pkgs, lib, config, ... }:
{
  modules = [
    ../../modules/services/firefox.nix
  ];
  home.packages = [
    pkgs.ipmitool
    # TODO: pkgs.ipmiview
    pkgs.skopeo
    pkgs.robo3t
  ]
  ++ (map (file: pkgs.writeScriptBin (builtins.baseNameOf file) (builtins.readFile file)) [
    ./bash/Majordomo_LLC_Root_CA.crt.sh
    ./bash/mj-hosts.sh
    ./bash/mjru-alerta
    ./bash/mjru-auth
    ./bash/mjru-dns
    ./bash/mjru-docker
    ./bash/mjru-fetch-history
    ./bash/mjru-flake
    ./bash/mjru-git-clone.sh
    ./bash/mjru-grafana
    ./bash/mjru-hms-migrate-web-account
    ./bash/mjru-infa
    ./bash/mjru-office
    ./bash/mjru-vpn.sh
  ]);

  home.file = {
    ".ssh/known_hosts" = {
      force = true;
      text = ''
        gitlab.corp1.majordomo.ru ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJw9vd+rL+MwVdVKSKW32+k6irAULLUFv5dmRUve2nUW
        gitlab.corp1.majordomo.ru ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIpKca//ukVhXODbccv/mv4oG74h8jyNQmF7ZbWd/qaolkBv0ptb/ocPc47+btv+FQTx3Fj/cPyi83kwf3ow7C8=
        gitlab.intr ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJw9vd+rL+MwVdVKSKW32+k6irAULLUFv5dmRUve2nUW
      '';
    };
  };

  programs.bash = {
    bashrcExtra = ''
      . ${./mjru.bash}
    '';
  };

  programs.ssh = {
    extraConfig = ''
      Host gitlab.intr
      User git
      IdentityFile /home/oleg/.ssh/id_rsa_gitlab_intr_nopass

      Host gitlab.corp1.majordomo.ru
      User git
      IdentityFile /home/oleg/.ssh/id_rsa_gitlab_intr_nopass

      Host *.intr
      User root
      IdentityFile /home/oleg/.ssh/id_rsa_majordomo_eng
      PubkeyAcceptedKeyTypes=+ssh-rsa
      HostKeyAlgorithms=+ssh-rsa
    '';
  };

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "24.05";
}
