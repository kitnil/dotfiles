# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ ... }:

{
  users.users.taskexecutor = {
    isNormalUser = true;
    uid = 1001;
    initialPassword = "taskexecutor";
  };

  users.groups.users = {
    members = [ "taskexecutor" ];
  };

  security.pki.certificates = [
    ''
      -----BEGIN CERTIFICATE-----
      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
      C17nt7CIsj9xIxtaQLPyGA==
      -----END CERTIFICATE-----
    ''
  ];

  services = {
    taskexecutor-nginx = {
      enable = true;
    };
  };
}
