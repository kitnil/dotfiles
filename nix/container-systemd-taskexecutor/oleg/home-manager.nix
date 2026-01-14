{ pkgs
, python-taskexecutor
, python-taskexecutor-wrapper
, lib
, config
, ... }:

{
  imports = [
    ../../modules/services/vendir.nix
  ];

  home.username = "taskexecutor";
  home.homeDirectory = "/home/taskexecutor";
  manual.manpages.enable = false;

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

  home.packages = with pkgs; [
    python-taskexecutor
    python-taskexecutor-wrapper
  ];

  services.vendir.enable = true;
}
