{ pkgs, lib, config, options, ... }:

with lib;

let
  yaml = pkgs.formats.yaml { };
  cfg = config.services.kube-scheduler;
in
{
  options.services.kube-scheduler = {
    manifest = mkOption {
      type = types.attrs;
      default = {
        apiVersion = "v1";
        kind = "Pod";
        metadata = { name = "static-kube-scheduler"; };
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
                        config.systemd.services.kube-scheduler.serviceConfig.ExecStart)))));
            files =
              unique
                (filter
                  (value:
                    hasPrefix "/nix/store" value || hasPrefix "/etc/kubernetes" value)
                  (fold
                    (file: files: files ++ file)
                    [
                      config.services.kubernetes.scheduler.kubeconfig.caFile
                      config.services.kubernetes.scheduler.kubeconfig.certFile
                      config.services.kubernetes.scheduler.kubeconfig.keyFile
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
                command = ["kube-scheduler"] ++ args;
                image = "k8s.gcr.io/kube-scheduler:v1.25.4";
                name = "kube-scheduler";
                env = [
                  {
                    name = "TZ";
                    value = config.time.timeZone;
                  }
                ];
                ports = [
                  {
                    containerPort = 10251;
                    name = "kube-scheduler";
                    protocol = "TCP";
                  }
                ];
                volumeMounts =
                  map
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
                    files;
              }
            ];
            volumes =
              map
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
                files;
          };
      };
    };
  };
  config = lib.mkIf config.services.kubernetes.scheduler.enable {
    environment.etc."kubernetes/manifests/kube-scheduler.yaml" = {
      source = yaml.generate "kube-scheduler.yaml" cfg.manifest;
    };
    systemd.services.kube-scheduler = {
      enable = false;
    };
  };
}
