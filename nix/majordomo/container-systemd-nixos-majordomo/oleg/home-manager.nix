{ pkgs, lib, config, python-with-packages, ... }:

let
  emacs-with-packages = with pkgs; ((emacsPackagesFor emacs-pgtk).emacsWithPackages (
    epkgs: [
      epkgs.deadgrep
      epkgs.edit-indirect
      epkgs.magit
      epkgs.helm
      epkgs.helm-projectile
      epkgs.lsp-mode
      epkgs.lua-mode
      epkgs.ivy
      epkgs.groovy-mode
      epkgs.nginx-mode
      epkgs.nix-mode
      epkgs.projectile
      epkgs.yaml-mode
      epkgs.wgrep
    ]
  ));
in
{
  imports = [
    ../../modules/services/firefox.nix
    ./private.nix
  ];
  home.packages = [
    emacs-with-packages
    pkgs.python-with-packages

    pkgs.alacritty
    pkgs.fd
    pkgs.file
    pkgs.fluxcd
    pkgs.jq
    pkgs.fuzzel
    pkgs.ipmitool
    pkgs.ipmiview
    pkgs.jetbrains.pycharm-oss
    pkgs.kubectl
    pkgs.mariadb.client
    pkgs.pass
    pkgs.ripgrep
    pkgs.robo3t
    pkgs.rsync
    pkgs.skopeo
    pkgs.xwayland-satellite
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

  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
    size = 32;
  };

  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        TERM = "xterm-256color";
      };
      font = {
        size = 10;
      };
      general = {
        live_config_reload = false;
      };
      colors = {
        primary = {
          foreground = "0xFFFFFF";
          background = "0x000000";
        };
        normal = {
          black = "0x000000";
          red = "0xB21818";
          green = "0x66CD00";
          yellow = "0xB26818";
          blue = "0x4169e1";
          magenta = "0xB218B2";
          cyan = "0x18B2B2";
          white = "0xB2B2B2";
        };
        bright = {
          black = "0x686868";
          red = "0xFF5454";
          green = "0x54FF54";
          yellow = "0xEEC900";
          blue = "0x5454FF";
          magenta = "0xFF54FF";
          cyan = "0x54FFFF";
          white = "0xFFFFFF";
        };
      };
    };
  };
  programs.fuzzel = {
    enable = true;
    settings = {
      border = {
        radius = 8;
      };
      colors = {
        background = "#000000ff";
        text = "#ffffffff";
        selection = "#2e8b57ff";
        selection-text = "#ffffffff";
        match = "#83a598ff";
        selection-match = "#ebdbb2ff";
        border = "#2e8b57ff";
      };
    };
  };

  programs.ssh = {
    enable = true;
  };

  systemd.user.services.xwayland-satellite = {
    Unit = {
      Description = "Xwayland-Satellite terminal";
      StartLimitBurst = 5;
      StartLimitIntervalSec = 10;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
    Service = {
      Environment = [
        "XDG_RUNTIME_DIR=/mnt/guix/run/user/%U"
        "WAYLAND_DISPLAY=wayland-1"
      ];
      ExecStart = "${pkgs.xwayland-satellite}/bin/xwayland-satellite";
      Type = "simple";
      Restart = "always";
      RestartSec = "2s";
    };
  };

  programs.bash = {
    enable = true;
    bashrcExtra = ''
      . ${./mjru.bash}
    '';
  };

  programs.k9s = {
    enable = true;
    settings = {
      k9s = {
        disablePodCounting = false;
        imageScans = {
          enable = false;
          exclusions = { labels = {}; namespaces = []; };
        };
        liveViewAutoRefresh = false;
        logger = {
          buffer = 5000;
          showTime = false;
          sinceSeconds = -1;
          tail = 100;
          textWrap = false;
        };
        maxConnRetry = 5;
        noExitOnCtrlC = false;
        readOnly = false;
        refreshRate = 2;
        screenDumpDir = "/home/oleg/.local/state/k9s/screen-dumps";
        shellPod = {
          image = "busybox:latest";
          limits = { cpu = "100m"; memory = "100Mi"; };
          namespace = "default";
        };
        skipLatestRevCheck = false;
        thresholds = {
          cpu = { critical = 90; warn = 70; };
          memory = { critical = 90; warn = 70; };
        };
        ui = {
          crumbsless = false;
          defaultsToFullScreen = false;
          enableMouse = false;
          headless = false;
          logoless = false;
          noIcons = false;
          reactive = false;
          skin = "everforest-light";
        };
      };
    };
    plugin = {
      plugins = {
        cert = {
          args = [
            "-c"
            "kubectl -n \$NAMESPACE get -o json secret \$NAME | jq '.data[\"tls.crt\"]' --raw-output | base64 -d | openssl x509 -text | less"
          ];
          background = false;
          command = "sh";
          confirm = false;
          description = "Show certificate in secret";
          scopes = [ "secrets" ];
          shortCut = "Shift-T";
        };
        getall-ns = {
          args = [ "-c" "kubectl get all -n \$NAME | less" ];
          background = false;
          command = "sh";
          confirm = false;
          description = "Get All Resources in NS";
          scopes = [ "namespaces" ];
          shortCut = "Shift-A";
        };
        node-ssh = {
          args = [
            "-l"
            "-c"
            "set -x; ssh \${NAME}.intr; read"
          ];
          background = false;
          command = "sh";
          description = "ssh to node";
          scopes = [ "nodes" ];
          shortCut = "Shift-S";
        };
        reconcile-git = {
          args = [ "-c" "flux reconcile source git -n \$NAMESPACE \$NAME | less" ];
          background = false;
          command = "sh";
          confirm = false;
          description = "Flux reconcile";
          scopes = [ "gitrepositories" ];
          shortCut = "Shift-R";
        };
        reconcile-hr = {
          args = [
            "-c"
            "flux reconcile helmrelease -n \$NAMESPACE \$NAME --with-source | less"
          ];
          background = false;
          command = "sh";
          confirm = false;
          description = "Flux reconcile";
          scopes = [ "helmreleases" ];
          shortCut = "Shift-R";
        };
        reconcile-ks = {
          args = [
            "-c"
            "flux reconcile kustomization -n \$NAMESPACE \$NAME --with-source | less"
          ];
          background = false;
          command = "sh";
          confirm = false;
          description = "Flux reconcile";
          scopes = [ "kustomizations" ];
          shortCut = "Shift-R";
        };
        terraform-plan = {
          args = [
            "-c"
            "kubectl -n \$NAMESPACE get -o json secret \$NAME | jq .data.tfplan --raw-output | base64 -d | gzip -d | gzip -d | less"
          ];
          background = false;
          command = "sh";
          confirm = false;
          description = "Terraform show plan";
          scopes = [ "secrets" ];
          shortCut = "Shift-P";
        };
        terraform-state = {
          args = [
            "-c"
            "kubectl -n \$NAMESPACE get -o json secret \$NAME | jq .data.tfstate --raw-output | base64 -d | gzip -d | less"
          ];
          background = false;
          command = "sh";
          confirm = false;
          description = "Terraform show state";
          scopes = [ "secrets" ];
          shortCut = "Shift-S";
        };
        toggle-helmrelease = {
          args = [
            "-c"
            "flux \$([ \$(kubectl get helmreleases -n \$NAMESPACE \$NAME -o=custom-columns=TYPE:.spec.suspend | tail -1) = \"true\" ] && echo \"resume\" || echo \"suspend\") helmrelease -n \$NAMESPACE \$NAME | less"
          ];
          background = false;
          command = "sh";
          confirm = true;
          description = "Toggle to suspend or resume a HelmRelease";
          scopes = [ "helmreleases" ];
          shortCut = "Shift-T";
        };
        toggle-kustomization = {
          args = [
            "-c"
            "flux \$([ \$(kubectl get kustomizations -n \$NAMESPACE \$NAME -o=custom-columns=TYPE:.spec.suspend | tail -1) = \"true\" ] && echo \"resume\" || echo \"suspend\") kustomization -n \$NAMESPACE \$NAME | less"
          ];
          background = false;
          command = "sh";
          confirm = true;
          description = "Toggle to suspend or resume a Kustomization";
          scopes = [ "kustomizations" ];
          shortCut = "Shift-T";
        };
        user-shell = {
          args = [
            "-c"
            "# set -o nounset -o errexit -o pipefail -o xtrace\nset -o xtrace\nget_container()\n{\n    kubectl \"--namespace=\${NAMESPACE}\" get pod -o json \"\$NAME\" | jq --raw-output '.spec.containers[] | .name' | fzf\n}\ncontainer=\"\$(get_container)\"\ncase \"\$container\" in\n    guix)\n        kubectl exec \"--namespace=\${NAMESPACE}\" --tty=true --stdin=true \"pod/\${NAME}\" \"--container=\${container}\" -- /run/setuid-programs/sudo -u oleg -i sh -l\n        ;;\n    nixos|archlinux|kali-rolling)\n        kubectl exec \"--namespace=\${NAMESPACE}\" --tty=true --stdin=true \"pod/\${NAME}\" \"--container=\${container}\" -- /run/current-system/sw/bin/machinectl shell oleg@\n        ;;\nesac\n"
          ];
          background = false;
          command = "bash";
          confirm = false;
          description = "User Shell";
          scopes = [ "pods" ];
          shortCut = "Shift-B";
        };
      };
    };
    skins = {
      transparent = {
        k9s = {
          body = { bgColor = "default"; };
          dialog = {
            bgColor = "default";
            fieldFgColor = "default";
            labelFgColor = "default";
          };
          frame = {
            crumbs = { bgColor = "default"; };
            menu = { fgColor = "default"; };
            title = { bgColor = "default"; counterColor = "default"; };
          };
          info = { sectionColor = "default"; };
          prompt = { bgColor = "default"; };
          views = {
            charts = { bgColor = "default"; };
            logs = {
              bgColor = "default";
              indicator = {
                bgColor = "default";
                toggleOffColor = "default";
                toggleOnColor = "default";
              };
            };
            table = {
              bgColor = "default";
              header = { bgColor = "default"; fgColor = "default"; };
            };
            xray = { bgColor = "default"; };
            yaml = { colonColor = "default"; valueColor = "default"; };
          };
        };
      };
    };
  };

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    enableBashIntegration = true;
    defaultCacheTtl = 172800;
    defaultCacheTtlSsh = 172800;
    maxCacheTtl = 172800;
    maxCacheTtlSsh = 172800;
    grabKeyboardAndMouse = false;
    pinentry = {
      package = pkgs.pinentry-curses;
    };
    extraConfig = ''
      allow-preset-passphrase
    '';
  };

  home.file = {
    ".my.cnf" = {
      text = ''
        [client]
        skip-ssl = true
      '';
    };
  };

  home.file = {
    ".pythonrc" = {
      text = builtins.readFile ./dot_pythonrc;
    };
  };

  home.file = {
    ".mozilla/native-messaging-hosts/passff.json" = {
      text = builtins.toJSON {
        allowed_extensions = [ "passff@wugi.info" ];
        description = "Host for communicating with zx2c4 pass";
        name = "passff";
        path = "${pkgs.passff-host}/share/passff-host/passff.py";
        type = "stdio";
      };
    };
    ".gitconfig" = {
      text = ''
        [user]
          email = go.wigust@gmail.com
          name = Oleg Pykhalov
      '';
    };
    ".emacs".source = ../../../../guix/dot_emacs;
    ".emacs.d/modules/audio.el".source = ../../../../guix/private_dot_emacs.d/modules/audio.el;
    ".emacs.d/modules/blog.el".source = ../../../../guix/private_dot_emacs.d/modules/blog.el;
    ".emacs.d/modules/c.el".source = ../../../../guix/private_dot_emacs.d/modules/c.el;
    ".emacs.d/modules/ci.el".source = ../../../../guix/private_dot_emacs.d/modules/ci.el;
    ".emacs.d/modules/compile.el".source = ../../../../guix/private_dot_emacs.d/modules/compile.el;
    ".emacs.d/modules/completion.el".source = ../../../../guix/private_dot_emacs.d/modules/completion.el;
    ".emacs.d/modules/copyright.el".source = ../../../../guix/private_dot_emacs.d/modules/copyright.el;
    ".emacs.d/modules/debbugs.el".source = ../../../../guix/private_dot_emacs.d/modules/debbugs.el;
    ".emacs.d/modules/debug.el".source = ../../../../guix/private_dot_emacs.d/modules/debug.el;
    ".emacs.d/modules/dired.el".source = ../../../../guix/private_dot_emacs.d/modules/dired.el;
    ".emacs.d/modules/elfeed.el".source = ../../../../guix/private_dot_emacs.d/modules/elfeed.el;
    ".emacs.d/modules/erc.el".source = ../../../../guix/private_dot_emacs.d/modules/erc.el;
    ".emacs.d/modules/ffap.el".source = ../../../../guix/private_dot_emacs.d/modules/ffap.el;
    ".emacs.d/modules/files.el".source = ../../../../guix/private_dot_emacs.d/modules/files.el;
    ".emacs.d/modules/ftp.el".source = ../../../../guix/private_dot_emacs.d/modules/ftp.el;
    ".emacs.d/modules/groovy.el".source = ../../../../guix/private_dot_emacs.d/modules/groovy.el;
    ".emacs.d/modules/guix.el".source = ../../../../guix/private_dot_emacs.d/modules/guix.el;
    ".emacs.d/modules/haskell.el".source = ../../../../guix/private_dot_emacs.d/modules/haskell.el;
    ".emacs.d/modules/hooks.el".source = ../../../../guix/private_dot_emacs.d/modules/hooks.el;
    ".emacs.d/modules/info.el".source = ../../../../guix/private_dot_emacs.d/modules/info.el;
    ".emacs.d/modules/java.el".source = ../../../../guix/private_dot_emacs.d/modules/java.el;
    ".emacs.d/modules/js.el".source = ../../../../guix/private_dot_emacs.d/modules/js.el;
    ".emacs.d/modules/keys.el".source = ../../../../guix/private_dot_emacs.d/modules/keys.el;
    ".emacs.d/modules/kubernetes.el".source = ../../../../guix/private_dot_emacs.d/modules/kubernetes.el;
    ".emacs.d/modules/lisp.el".source = ../../../../guix/private_dot_emacs.d/modules/lisp.el;
    ".emacs.d/modules/llm.el".source = ../../../../guix/private_dot_emacs.d/modules/llm.el;
    ".emacs.d/modules/lsp.el".source = ../../../../guix/private_dot_emacs.d/modules/lsp.el;
    ".emacs.d/modules/lsp-nixd.el".source = ../../../../guix/private_dot_emacs.d/modules/lsp-nixd.el;
    ".emacs.d/modules/mail.el".source = ../../../../guix/private_dot_emacs.d/modules/mail.el;
    ".emacs.d/modules/majordomo.el".source = ../../../../guix/private_dot_emacs.d/modules/majordomo.el;
    ".emacs.d/modules/ml.el".source = ../../../../guix/private_dot_emacs.d/modules/ml.el;
    ".emacs.d/modules/nav.el".source = ../../../../guix/private_dot_emacs.d/modules/nav.el;
    ".emacs.d/modules/nix.el".source = ../../../../guix/private_dot_emacs.d/modules/nix.el;
    ".emacs.d/modules/org.el".source = ../../../../guix/private_dot_emacs.d/modules/org.el;
    ".emacs.d/modules/outline.el".source = ../../../../guix/private_dot_emacs.d/modules/outline.el;
    ".emacs.d/modules/perl.el".source = ../../../../guix/private_dot_emacs.d/modules/perl.el;
    ".emacs.d/modules/po.el".source = ../../../../guix/private_dot_emacs.d/modules/po.el;
    ".emacs.d/modules/python.el".source = ../../../../guix/private_dot_emacs.d/modules/python.el;
    ".emacs.d/modules/rfc.el".source = ../../../../guix/private_dot_emacs.d/modules/rfc.el;
    ".emacs.d/modules/rust.el".source = ../../../../guix/private_dot_emacs.d/modules/rust.el;
    ".emacs.d/modules/scheme.el".source = ../../../../guix/private_dot_emacs.d/modules/scheme.el;
    ".emacs.d/modules/slack.el".source = ../../../../guix/private_dot_emacs.d/modules/slack.el;
    ".emacs.d/modules/snippets.el".source = ../../../../guix/private_dot_emacs.d/modules/snippets.el;
    ".emacs.d/modules/term.el".source = ../../../../guix/private_dot_emacs.d/modules/term.el;
    ".emacs.d/modules/text.el".source = ../../../../guix/private_dot_emacs.d/modules/text.el;
    ".emacs.d/modules/theme.el".source = ../../../../guix/private_dot_emacs.d/modules/theme.el;
    ".emacs.d/modules/time.el".source = ../../../../guix/private_dot_emacs.d/modules/time.el;
    ".emacs.d/modules/tramp.el".source = ../../../../guix/private_dot_emacs.d/modules/tramp.el;
    ".emacs.d/modules/twitch.el".source = ../../../../guix/private_dot_emacs.d/modules/twitch.el;
    ".emacs.d/modules/utils.el".source = ../../../../guix/private_dot_emacs.d/modules/utils.el;
    ".emacs.d/modules/version-control.el".source = ../../../../guix/private_dot_emacs.d/modules/version-control.el;
    ".emacs.d/modules/version-control-lexical.el".source = ../../../../guix/private_dot_emacs.d/modules/version-control-lexical.el;
    ".emacs.d/modules/web.el".source = ../../../../guix/private_dot_emacs.d/modules/web.el;
    ".emacs.d/modules/window.el".source = ../../../../guix/private_dot_emacs.d/modules/window.el;
    ".emacs.d/modules/yaml.el".source = ../../../../guix/private_dot_emacs.d/modules/yaml.el;
    ".emacs.d/modules/youtube.el".source = ../../../../guix/private_dot_emacs.d/modules/youtube.el;
  };

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "24.05";
}
