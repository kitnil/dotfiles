{ pkgs, lib, config, options, ... }:

with lib;

let
  yaml = pkgs.formats.yaml { };
  cfg = config.services.kube-apiserver;
in
{
  options.services.kube-apiserver = {
    manifest = mkOption {
      type = types.attrs;
      default = {
        apiVersion = "v1";
        kind = "Pod";
        metadata = { name = "static-kube-apiserver"; };
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
                        config.systemd.services.kube-apiserver.serviceConfig.ExecStart)))));
            files =
              unique
                (filter
                  (value:
                    hasPrefix "/nix/store" value || hasPrefix "/etc/kubernetes" value)
                  (map
                    (arg:
                      elemAt
                        (splitString "=" arg)
                        1)
                    args));
            replaceDotWithDash = file:
              concatStringsSep "-" (splitString "." (baseNameOf file));
          in {
            hostNetwork = true;
            containers = [
              {
                command = ["kube-apiserver"] ++ args;
                image = "k8s.gcr.io/kube-apiserver:v1.25.4";
                name = "kube-apiserver";
                env = [
                  {
                    name = "TZ";
                    value = config.time.timeZone;
                  }
                ];
                ports = [
                  {
                    containerPort = 6443;
                    name = "kube-apiserver";
                    protocol = "TCP";
                  }
                ];
                volumeMounts =
                  fold
                    (volumes: volume: volumes ++ volume)
                    [
                      {
                        name = "ca-certificates-crt";
                        mountPath = "/etc/ssl/certs/ca-certificates.crt";
                        readOnly = true;
                      }
                    ]
                    [
                      (map
                        (file:
                          {
                            name = replaceDotWithDash file;
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
                    name = "ca-certificates-crt";
                    hostPath = {
                      path = config.environment.etc."ssl/certs/ca-certificates.crt".source;
                      type = "File";
                    };
                  }
                ]
                [
                  (map
                    (file:
                      {
                        name = replaceDotWithDash file;
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
  config = lib.mkIf config.services.kubernetes.apiserver.enable {
    environment.etc."kubernetes/manifests/kube-apiserver.yaml" = {
      source = yaml.generate "kube-apiserver.yaml" cfg.manifest;
    };
    systemd.services.kube-apiserver = {
      enable = false;
    };
  };
}
