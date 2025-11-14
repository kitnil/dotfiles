{ pkgs, lib, config, options, ... }:

with lib;

let
  yaml = pkgs.formats.yaml { };
  cfg = config.services.kube-controller-manager;
in
{
  options.services.kube-controller-manager = {
    manifest = mkOption {
      type = types.attrs;
      default = {
        apiVersion = "v1";
        kind = "Pod";
        metadata = { name = "static-kube-controller-manager"; };
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
                        config.systemd.services.kube-controller-manager.serviceConfig.ExecStart)))));
            files =
              unique
                (filter
                  (value:
                    hasPrefix "/nix/store" value || hasPrefix "/etc/kubernetes" value)
                  (fold
                    (file: files: files ++ file)
                    [
                      # config.services.kubernetes.controllerManager.kubeconfig.caFile
                      config.services.kubernetes.controllerManager.kubeconfig.certFile
                      config.services.kubernetes.controllerManager.kubeconfig.keyFile
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
                command = ["kube-controller-manager"] ++ args;
                image = "k8s.gcr.io/kube-controller-manager:v1.25.4";
                name = "kube-controller-manager";
                env = [
                  {
                    name = "TZ";
                    value = config.time.timeZone;
                  }
                ];
                ports = [
                  {
                    containerPort = 10252;
                    name = "controller";
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
  config = lib.mkIf config.services.kubernetes.controllerManager.enable {
    environment.etc."kubernetes/manifests/kube-controller-manager.yaml" = {
      source = yaml.generate "kube-controller-manager.yaml" cfg.manifest;
    };
    systemd.services.kube-controller-manager = {
      enable = false;
    };
  };
}
