{ pkgs, python-taskexecutor, lib, config, ... }:

{
  home.username = "taskexecutor";
  home.homeDirectory = "/home/taskexecutor";
  manual.manpages.enable = false;

  home.packages = [
    python-taskexecutor
  ];

  services.vendir.enable = true;

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "22.05";
}
