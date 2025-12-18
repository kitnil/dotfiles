{ pkgs, lib, config, options, ... }:

with lib;

let
  yaml = pkgs.formats.yaml { };
  cfg = config.services.etcd;
in
{
  options.services.etcd = {
    manifest = mkOption {
      type = types.attrs;
      default = {
        apiVersion = "v1";
        kind = "Pod";
        metadata = { name = "static-etcd"; };
        spec =
          let
            args = tail
              (reverseList
                (fold
                  (x: xs: xs ++ x)
                  []
                  (map
                    (str: filter (str: str != "" && str != "\\") str)
                    (map
                      (str:
                        splitString " " str)
                      (splitString
                        "\n"
                        config.systemd.services.etcd.serviceConfig.ExecStart)))));
            files =
              unique
                (filter
                  (value:
                    hasPrefix "/nix/store" value || hasPrefix "/etc/kubernetes" value)
                  (fold
                    (file: files: files ++ file)
                    [
                      config.services.etcd.certFile
                      config.services.etcd.peerCertFile
                      config.services.etcd.peerKeyFile
                      config.services.etcd.peerTrustedCaFile
                      config.services.etcd.trustedCaFile
                    ]
                    [
                      (map
                        (arg:
                          let
                            str = splitString "=" arg;
                          in
                            if (length str) > 1
                            then
                              elemAt str 1
                            else
                              head str)
                        args)
                    ]));
            replaceDotWithDash = file:
              concatStringsSep "-" (splitString "." file);
          in {
            hostNetwork = true;
            containers = [
              {
                image = "gcr.io/etcd-development/etcd:v3.3.27";
                name = "etcd";
                env =
                  (attrValues
                    (mapAttrs
                      (name: value:
                        { inherit name value; })
                      (filterAttrs
                        (name: value: hasPrefix "ETCD_" name)
                        config.systemd.services.etcd.environment)))
                  ++ [
                    {
                      name = "TZ";
                      value = config.time.timeZone;
                    }
                  ];
                ports = [
                  {
                    containerPort = 2379;
                    name = "etcd-0";
                    protocol = "TCP";
                  }
                  {
                    containerPort = 2380;
                    name = "etcd-1";
                    protocol = "TCP";
                  }
                  {
                    containerPort = 2381;
                    name = "etcd-2";
                    protocol = "TCP";
                  }
                ];
                volumeMounts =
                  fold
                    (volumes: volume: volumes ++ volume)
                    [
                      {
                        name = "etcd-data";
                        mountPath = "/var/lib/etcd";
                      }
                    ]
                    [
                      (map
                        (file:
                          {
                            name =
                              let
                                name = replaceDotWithDash (baseNameOf file);
                              in
                                if (stringLength name) > 63
                                then
                                  substring
                                    (stringLength "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-")
                                    (stringLength name)
                                    name
                                else
                                  name;
                            mountPath = file;
                            readOnly = true;
                          })
                        files)
                    ];
              }
            ];
            volumes =
              fold
                (volumes: volume: volumes ++ volume)
                [
                  {
                    name = "etcd-data";
                    hostPath = {
                      path = "/var/lib/etcd";
                      type = "Directory";
                    };
                  }
                ]
                [
                  (map
                    (file:
                      {
                        name =
                          let
                            name = replaceDotWithDash (baseNameOf file);
                          in
                            if (stringLength name) > 63
                            then
                              substring
                                (stringLength "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-")
                                (stringLength name)
                                name
                            else
                              name;
                        hostPath = {
                          path = file;
                          type = "File";
                        };
                      })
                    files)
                ];
          };
      };
    };
  };
  config = lib.mkIf config.services.kubernetes.scheduler.enable {
    environment.etc."kubernetes/manifests/etcd.yaml" = {
      source = yaml.generate "etcd.yaml" cfg.manifest;
    };
    systemd.services.etcd = {
      enable = false;
    };
  };
}
