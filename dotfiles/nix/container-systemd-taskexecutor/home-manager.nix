{ pkgs, taskexecutor-pkgs, taskexecutor-packages, lib, config, ... }:

{
  home.username = "taskexecutor";
  home.homeDirectory = "/home/taskexecutor";
  manual.manpages.enable = false;

  home.packages = (with taskexecutor-pkgs; [
    procps
    coreutils
    curl
    findutils
    ripgrep
    gitMinimal
    gitaskpass
    gnugrep
    gnutar
    gnused
    gzip
    mariadb.client
    nss-certs
    openssh
    quota
    restic
    rsync
    shadow
  ]) ++ taskexecutor-packages;

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "22.05";
}
