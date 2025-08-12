# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, modulesPath, pkgs, nixpkgs, ... }:

{
  imports = [
    (modulesPath + "/virtualisation/docker-image.nix")
  ];

  boot.isContainer = true;
  boot.loader.initScript.enable = true;

  time.timeZone = "Europe/Moscow";

  networking = {
    hostName = ""; # empty
    useDHCP = false;
    useNetworkd = false;
    useHostResolvConf = false;
    firewall.enable = false;
    resolvconf.enable = false;
    extraHosts = ''
      192.168.0.148 example-tor-instance-tor-svc.tor-controller-instance
      192.168.0.196 workstation-kube1.workstation
    '';
  };

  programs.firejail = {
    enable = true;
  };

  virtualisation.docker = {
    enable = true;
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = false;
  services.openssh.settings = {
    PermitRootLogin = "prohibit-password";
  };

  services.openvpn.servers = {
    client = {
      config =
        let
          mjuh-utils-openvpn = pkgs.fetchFromGitHub {
            owner = "mjuh";
            repo = "utils-openvpn";
            rev = "901b406f27035c641683f8e869919b9e0eb28153";
            hash = "sha256-6cV7uPJbA5YHGgOu/xuKldUCSaiguzPc2nWQFsws3z8=";
          };
          caFile = "${mjuh-utils-openvpn}/etc/openvpn/majordomo-ca.cert";
        in ''
          client
          proto udp
          dev tapvpn
          verb 3

          remote 78.108.87.250 1194 udp
          cipher AES-256-GCM
          data-ciphers AES-256-GCM

          remote-cert-tls server
          ca "${caFile}"
          auth SHA1
          script-security 3
          auth-nocache
          auth-retry nointeract
          ping 5
          ping-restart 10
          auth-user-pass /etc/openvpn/login.conf
        '';
    };
  };

  systemd.services.openvpn-client = {
    unitConfig = {
      ConditionPathExists = [ "/etc/openvpn/login.conf" ];
    };
  };

  services.journald.console = "/dev/tty";
  services.journald.extraConfig = "SystemMaxUse=100M";

  # Define a user account. Don't forget to set a password with ‘passwd’.

  users.users.root.password = ""; # Empty password.

  users.users.oleg = {
    isNormalUser = true;
    extraGroups = [
      "docker"
      "wheel" # Enable ‘sudo’ for the user.
    ];
    uid = 1000;
    initialPassword = "oleg";
  };

  users.groups.users = {
    name = "users";
    members = [ "oleg" ];
    gid = lib.mkForce 998;
  };

  security.sudo.extraConfig = ''
    oleg ALL = (root) NOPASSWD:ALL
  '';

  # but NIX_PATH is still used by many useful tools, so we set it to the same value as the one used by this flake.
  # Make `nix repl '<nixpkgs>'` use the same nixpkgs as the one used by this flake.
  environment.etc."nix/inputs/nixpkgs".source = "${nixpkgs}";
  nix = {
    package = pkgs.nixVersions.git;
    extraOptions = "experimental-features = nix-command flakes";

    # make `nix run nixpkgs#nixpkgs` use the same nixpkgs as the one used by this flake.
    registry.nixpkgs.flake = nixpkgs;
    channel.enable = false; # remove nix-channel related tools & configs, we use flakes instead.

    settings = {
      trusted-users = [ "root" ];
      # https://github.com/NixOS/nix/issues/9574
      nix-path = lib.mkForce "nixpkgs=/etc/nix/inputs/nixpkgs";
    };
  };

  environment.systemPackages = with pkgs; [
    gitAndTools.git
  ];

  system.extraSystemBuilderCmds =
    let
      inherit (pkgs) bashInteractive util-linuxMinimal runtimeShell writeScript;
      entrypoint = writeScript "entrypoint.sh" ''
        #!${runtimeShell}
        set -o nounset -o errexit -o pipefail -o xtrace

        PATH=${util-linuxMinimal}/bin
        export PATH

        # Keyboard and mouse access are essential components for efficient operation
        # within the Sway window manager environment. Running 'udevadm trigger' will
        # grant access to the keyboard and mouse. You can verify their addition by
        # using 'libinput list-devices'."
        mount -o remount,rw /sys

        umount /sys/fs/cgroup
        mount -t cgroup2 -o rw,relatime,nsdelegate,memory_recursiveprot cgroup2 /sys/fs/cgroup

        # https://lore.kernel.org/lkml/87tvsrjai0.fsf@xmission.com/T/
        mount -t proc none /proc

        exec /init
      '';
    in ''
      ln -s ${entrypoint} $out/entrypoint.sh
      ln -s ${bashInteractive}/bin/bash $out/bin/bash
    '';

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      dejavu_fonts
      wqy_zenhei
    ];
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
