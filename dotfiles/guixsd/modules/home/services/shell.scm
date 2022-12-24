(define-module (home services shell)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:use-module (gnu packages dhall)
  #:use-module (gnu packages wm)
  #:export (home-bash-service
            home-direnv-service
            home-inputrc-service
            home-bin-service
            home-parallel-service))


;;;
;;; Bash
;;;

(define home-bash-service
  (simple-service 'bash-config
                  home-files-service-type
                  (append (list `(".bashrc" ,(local-file (string-append %project-directory "/dot_bashrc")))
                                `(".bash_profile" ,(local-file (string-append %project-directory "/dot_bash_profile")))
                                `(".bash_completion" ,(local-file (string-append %project-directory "/dot_bash_completion")))
                                `(".bash_guix" ,(local-file (string-append %project-directory "/dot_bash_guix")))
                                `(".bash_vterm" ,(local-file (string-append %project-directory "/dot_bash_vterm")))
                                `(".local/share/bash-completion/completions/lexicon" ,(local-file (string-append %project-directory "/dot_local/share/bash-completion/completions/lexicon")))
                                `(".local/share/bash-completion/completions/herd" ,(local-file (string-append %project-directory "/dot_local/share/bash-completion/completions/herd")))
                                `(".local/share/bash-completion/completions/mail" ,(local-file (string-append %project-directory "/dot_local/share/bash-completion/completions/mail")))
                                `(".local/share/bash-completion/completions/connect" ,(local-file (string-append %project-directory "/dot_local/share/bash-completion/completions/connect"))))
                          (map (lambda (file-name)
                                 `(,(string-append ".bash.d/" file-name) ,(local-file (string-append %project-directory "/dot_bash.d/" file-name))))
                               '("bash.scm"
                                 "mjru.bash")))))

(define home-inputrc-service
  (simple-service 'inputrc-config
                  home-files-service-type
                  (list `(".inputrc" ,(local-file (string-append %project-directory "/dot_inputrc"))))))

(define home-direnv-service
  (simple-service 'direnv-config
                  home-files-service-type
                  (list `(".direnvrc" ,(local-file (string-append %project-directory "/dot_direnvrc"))))))

(define xmenu.sh
  (computed-file
   "xmenu.sh"
   #~(begin
       (use-modules (ice-9 rdelim)
                    (ice-9 popen))
       (let* ((port (open-pipe* OPEN_READ #$(file-append dhall "/bin/dhall")
                                "text" "--file" #$(local-file (string-append %project-directory "/dhall/xmenu.dhall"))))
              (output (read-string port)))
         (close-port port)
         (call-with-output-file #$output
           (lambda (port)
             (display (string-trim-right output #\newline) port)))
         (chmod #$output #o555)))))

(define home-bin-service
  (simple-service 'bin-config
                  home-files-service-type
                  (append
                   (list `(".local/bin/xmenu.sh" ,xmenu.sh))
                   (map (lambda (program)
                          `(,(string-append ".local/bin/" program)
                            ,(local-file (string-append %project-directory "/dot_local/bin/executable_" program)
                                         #:recursive? #t)))
                        '("alerta-close"
                          "ansible-update-ssh-known-hosts"
                          "backup"
                          "bash-notify"
                          "block-ip"
                          "blog"
                          "brctl-start"
                          "brightness"
                          "cerb"
                          "checkssl.sh"
                          "chroot-games.sh"
                          "clone-gitlab.intr.scm"
                          "color-converter"
                          "connect"
                          "convert-music"
                          "covid19"
                          "debian-chroot.sh"
                          "debian.sh"
                          "dns"
                          "dnscheck"
                          "dotfiles"
                          "elk-index-youtube"
                          "elogind-sway"
                          "emacs-guix-log"
                          "emacs-org-capture"
                          "emc"
                          "eww"
                          "fedora"
                          "ff"
                          "ffmpeg-hwaccel"
                          "ffmpeg-software"
                          "ffmpeg-youtube"
                          "fileshelter"
                          "firefox-guile"
                          "firefox-nix"
                          "firefox-esr"
                          "firefox-temp-profile"
                          "firefox-youtube-chat"
                          "gita-dist"
                          "gita-kitnil"
                          "gita-mjru"
                          "gita-src"
                          "git-changelog-symlink-init.sh"
                          "git-gitlab"
                          "github-create-repository"
                          "gitlab-runner-service"
                          "git-mirror"
                          "git-diff-date"
                          "git-pass-secrets"
                          "git-pure"
                          "git-statistics"
                          "gnus"
                          "godaddy"
                          "gpg-unlock"
                          "grafana"
                          "guile-git-list-commiters"
                          "guix-clean.sh"
                          "guix-custom-refresh"
                          "guix-environment.sh"
                          "guix-git-reset-to-current-channel"
                          "guix-latest"
                          "guix-my-services"
                          "guix-show"
                          "guix-update.sh"
                          "guix-weather-x86_64.sh"
                          "hms"
                          "import-cert.sh"
                          "iommu.sh"
                          "iproute2-bridge"
                          "jenkins"
                          "jenkins-active-jobs"
                          "jenkins-lastbuild"
                          "jenkins-local"
                          "jenkins-nix-version"
                          "lint"
                          "lists.sh"
                          "magit"
                          "mail"
                          "Majordomo_LLC_Root_CA.crt.sh"
                          "messages2notify-send"
                          "mj-hosts.sh"
                          "mjru-auth"
                          "mjru-alerta"
                          "mjru-dns"
                          "mjru-docker"
                          "mjru-fetch-history"
                          "mjru-flake"
                          "mjru-git-clone.sh"
                          "mjru-github-projects.scm"
                          "mjru-grafana"
                          "mjru-infa"
                          "mjru-office"
                          "mjru-vpn.sh"
                          "monitor"
                          "monitoror"
                          "mpvctl"
                          "mpv-wrapper"
                          "my-docker"
                          "my-swank"
                          "my-xorg"
                          "nginx-server-name"
                          "nixos.sh"
                          "nix-repl"
                          "oracle"
                          "pers"
                          "peertube"
                          "prometheus-billing2"
                          "pulseaudio-switch-sink.sh"
                          "qemu-cdrom.sh"
                          "qemu-cdrom-vnc.sh"
                          "qemu-cdrom-win.sh"
                          "qemu-freebsd.sh"
                          "qemu-vpn.sh"
                          "record-video.sh"
                          "record-window"
                          "record-window-gif.sh"
                          "reevefresh"
                          "rofi-mycli"
                          "rofi-stumpwm"
                          "rofi-xterm"
                          "rofi-billing2-server"
                          "rss"
                          "run-docker"
                          "run-emacs"
                          "run-in-xterm"
                          "run-jenkins"
                          "run-jenkins-agent"
                          "run-kresd"
                          "run-nix-daemon"
                          "run-openvpn"
                          "run-place-existing-windows"
                          "run-stumpwm"
                          "sbcl"
                          "scan"
                          "shop"
                          "shutdown"
                          "src-clean"
                          "src-import.scm"
                          "ssh-keyscan.scm"
                          "ssh-aliases"
                          "ssh-sudo"
                          "ssh-vm"
                          "ssl"
                          "toggle-input-method.sh"
                          "tome4-docker"
                          "tranfser-curl"
                          "twitch.scm"
                          "ubuntu"
                          "vault"
                          "vfio.sh"
                          "video"
                          "vnc"
                          "vncview-5901.sh"
                          "volume-switch.sh"
                          "wallhaven"
                          "wallpaper"
                          "web-docker-pull"
                          "wi-emacs-shell.sh"
                          "wi-emacs-wget"
                          "wi-image-rotate.sh"
                          "wi-qemu-epson.sh"
                          "wi-show-colors"
                          "xclip-mpv.sh"
                          "xdg-open"
                          "xterm-dark"
                          "yeastizzy"
                          "youtube-build"
                          "youtube-dl-json"
                          "youtube-dl-music-play-url"
                          "youtube-scm"
                          "yt")))))

(define home-parallel-service
  (simple-service 'parallel-config
                  home-activation-service-type
                  #~(begin
                      (let* ((%home
                              (and=> (getenv "HOME")
                                     (lambda (home)
                                       home)))
                             (parallel (string-append %home "/.parallel")))
                        (unless (file-exists? parallel)
                          (mkdir parallel))
                        (call-with-output-file (string-append parallel "/runs-without-willing-to-cite")
                          (lambda (port)
                            (display "6\n" port)))))))
