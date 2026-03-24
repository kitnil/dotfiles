(define-module (wugi home config workstation)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services mail)
  #:use-module (gnu home services niri)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels workstation)
  #:use-module (wugi home config)
  #:use-module (wugi home services audio)
  #:use-module (wugi home services databases)
  #:use-module (wugi home services desktop)
  #:use-module (wugi home services dns)
  #:use-module (wugi home services emacs)
  #:use-module (wugi home services gdb)
  #:use-module (wugi home services gnupg)
  #:use-module (wugi home services groovy)
  #:use-module (wugi home services gtk)
  #:use-module (wugi home services guile)
  #:use-module (wugi home services haskell-apps)
  #:use-module (wugi home services kodi)
  #:use-module (wugi home services linux)
  #:use-module (wugi home services lisp)
  #:use-module (wugi home services mail)
  #:use-module (wugi home services mime)
  #:use-module (wugi home services nano)
  #:use-module (wugi home services nix)
  #:use-module (wugi home services python)
  #:use-module (wugi home services rust-apps)
  #:use-module (wugi home services shell)
  #:use-module (wugi home services terminals)
  #:use-module (wugi home services tmux)
  #:use-module (wugi home services version-control)
  #:use-module (wugi home services video)
  #:use-module (wugi home services web)
  #:use-module (wugi manifests workstation)
  #:use-module (wugi utils)
  #:export (%workstation-home-environment))

(define (manifest->packages manifest)
  "Return the list of packages in MANIFEST."
  (filter-map (lambda (entry)
                (let ((item (manifest-entry-item entry)))
                  (if (or (package? item) (inferior-package? item)) item #f)))
              (manifest-entries manifest)))

(define (packages-from-manifest manifest)
  "Return the list of packages in loaded MANIFEST."
  (let* ((user-module (make-user-module '((guix profiles) (gnu))))
         (manifest    (load* manifest user-module)))
    (manifest->packages manifest)))

(define waybar-configuration-service
  (simple-service
   'waybar-config
   home-files-service-type
   (list `(".config/waybar/config.jsonc"
           ,(local-file (string-append %distro-directory
                                       "/dot_config/waybar/config.jsonc")))
         `(".config/waybar/style.css"
           ,(local-file (string-append %distro-directory
                                       "/dot_config/waybar/style.css"))))))

(define mako-configuration-service
  (simple-service
   'mako-config
   home-files-service-type
   (list `(".config/mako/config"
           ,(local-file
             (string-append %distro-directory
                            "/dot_config/mako/config"))))))

(define aichat-configuration-service
  (simple-service
   'aichat-config
   home-files-service-type
   (list `(".config/aichat/config.yaml"
           ,(local-file
             (string-append %distro-directory
                            "/dot_config/aichat/config.yaml"))))))

(define niri-configuration-service
  (simple-service 'niri-config
                  home-files-service-type
                  (list `(".config/niri/config.kdl"
                          ,(local-file
                            (string-append %distro-directory
                                           "/dot_config/niri/config.kdl"))))))

(define fuzzel-configuration-service
  (simple-service
   'fuzzel-config
   home-files-service-type
   (list `(".config/fuzzel/fuzzel.ini"
           ,(local-file
             (string-append %distro-directory
                            "/dot_config/fuzzel/fuzzel.ini"))))))

(define fuzzel-service
  (simple-service
   'fuzzel
   home-profile-service-type
   (list fuzzel)))

(define gnupg-configuration-service
  (simple-service 'gnupg-config
                  home-files-service-type
                  (list `(".gnupg/gpg.conf"
                          ,(local-file
                            (string-append %distro-directory
                                           "/private_dot_gnupg/gpg.conf")))
                        `(".gnupg/gpg-agent.conf"
                          ,(mixed-text-file "gpg-agent.conf" "\
no-grab
pinentry-program " (file-append pinentry "/bin/pinentry") "
pinentry-timeout 5
default-cache-ttl 172800
default-cache-ttl-ssh 172800
max-cache-ttl 172800
max-cache-ttl-ssh 172800
allow-preset-passphrase")))))

(define pipewire-configuration-service
  ;; default.clock.min-quantum configuration is suggested in
  ;; https://www.reddit.com/r/linux_gaming/comments/1gy347h/newbie_here_ive_tried_almost_all_fixes_theres/
  (simple-service 'pipewire-config
                  home-files-service-type
                  (list `(".config/pipewire/pipewire.conf.d/99-low-latency.conf"
                          ,(mixed-text-file "pipewire-99-low-latency.conf" "\
context.properties = {
    default.clock.rate = 48000
    default.clock.allowed-rates = [ 44100 48000 ]
    default.clock.min-quantum = 1024
}
")))))

(define* (runc-fuzzel container-name #:key launch-prefix)
  #~(begin
      (use-modules (json)
                   (ice-9 popen)
                   (ice-9 rdelim))
      (define (focused-output)
        (let* ((port (open-pipe* OPEN_READ "niri" "msg" "--json" "focused-output"))
               (output (read-string port)))
          (json-string->scm (string-trim-right output #\newline))))
      (define (output-name output)
        (string->symbol (assoc-ref output "name")))
      (define (logical-scale output)
        (assoc-ref (assoc-ref output "logical")
                   "scale"))
      (let ((output (focused-output)))
        (apply execl
               (append (list "/run/privileged/bin/sudo" "runc"
                             "/run/current-system/profile/sbin/runc" "exec"
                             "--env" (and=> (getenv "USER")
                                            (lambda (user)
                                              (string-append "USER=" user)))
                             "--env" (and=> (getenv "WAYLAND_DISPLAY")
                                            (lambda (display)
                                              (string-append "WAYLAND_DISPLAY=" display)))
                             "--env" "DESKTOP_SESSION=niri"
                             "--env" "XDG_CURRENT_DESKTOP=niri"
                             "--env" "XDG_SESSION_DESKTOP=niri"
                             "--env" "XDG_SESSION_TYPE=wayland"
                             "--env" (and=> (getenv "GTK_THEME")
                                            (lambda (theme)
                                              (string-append "GTK_THEME=" theme)))
                             "--env" "XDG_RUNTIME_DIR=/mnt/guix/run/user/1000")
                       (list "--env" (string-append "GUIX_DBUS_SESSION_BUS_ADDRESS="
                                                    "unix:path="
                                                    "/mnt/guix"
                                                    (string-drop (getenv "DBUS_SESSION_BUS_ADDRESS")
                                                                 (string-length "unix:path="))))
                       '("--env" "DISPLAY=:0")
                       (case (output-name output)
                         ((DP-3) (list "--env" (string-append "GDK_SCALE="
                                                              (number->string (logical-scale output)))))
                         (else '()))
                       (list "--user=1000:998"
                             #$container-name
                             "/bin/sh" "-lc"
                             (string-join
                              (append (list "exec" "/usr/bin/env" "fuzzel")
                                      (if #$launch-prefix
                                          (list #$launch-prefix)
                                          '())))))))))

(define bin-configuration-service
  (simple-service 'bin-manual-scripts
                  home-files-service-type
                  (append (list `("bin/manual-scripts-root-02-net.sh"
                                  ,(local-file (string-append %distro-directory "/dotfiles/run/guix-workstation/03-net.sh")
                                               #:recursive? #t))
                                `("bin/manual-scripts-root-03-firefox-twitch-namespace.sh"
                                  ,(local-file (string-append %distro-directory "/dotfiles/run/guix-workstation/04-firefox-twitch-namespace.sh")
                                               #:recursive? #t))
                                `("bin/manual-scripts-root-04-mjru-net"
                                  ,(program-file "manual-scripts-root-04-mjru-net"
                                                 #~(let ((gateway "192.168.0.144"))
                                                     (for-each (lambda (network)
                                                                 (system* #$(file-append iproute "/sbin/ip") "route" "add" network "via" gateway))
                                                               '("172.16.103.0/24"
                                                                 "78.108.80.0/24"
                                                                 "78.108.88.0/24")))))
                                `("bin/manual-scripts-oleg-01-ssh.sh"
                                  ,(local-file (string-append %distro-directory "/dotfiles/run/guix-workstation/02-ssh.sh")
                                               #:recursive? #t))
                                `("bin/manual-scripts-oleg-02-gnupg.sh"
                                  ,(local-file (string-append %distro-directory "/dotfiles/run/guix-workstation/05-gnupg.sh")
                                               #:recursive? #t))
                                `("bin/fuzzel-nixos-workstation"
                                  ,(program-file "fuzzel-nixos-workstation"
                                                 (runc-fuzzel "nixos-workstation"
                                                              #:launch-prefix "--launch-prefix=/etc/profiles/per-user/oleg/bin/fuzzel-wrapper")))
                                `("bin/fuzzel-nixos-majordomo"
                                  ,(program-file "fuzzel-nixos-majordomo"
                                                 (runc-fuzzel "nixos-majordomo")))))))

(define host-namespace-configuration-service
  (simple-service 'bin-namespace-host
                  home-files-service-type
                  (list `("bin/namespace-host"
                          ,(program-file "namespace-host"
                                         #~(execl #$(file-append openssh "/bin/ssh")
                                                  "ssh" "-t" "192.168.0.192" "tmux"))))))

(define wl-mirror-configuration-service
  (simple-service 'bin-wl-mirror
                  home-files-service-type
                  (map (lambda (wayland-output)
                         (let ((file-name (string-downcase wayland-output)))
                           `(,(string-append "bin/" file-name)
                             ,(program-file file-name
                                            #~(execl #$(file-append wl-mirror "/bin/wl-mirror")
                                                     "wl-mirror"
                                                     "--scaling" "exact"
                                                     #$wayland-output)))))
                       '("HEADLESS-1"
                         "HEADLESS-2"
                         "HEADLESS-3"
                         "HEADLESS-4"
                         "HEADLESS-5"
                         "HEADLESS-6"
                         "HEADLESS-7"
                         "HEADLESS-8"))))

(define bemenu-configuration-service
  (simple-service 'bin-bemenu-scripts
                  home-files-service-type
                  (list `("bin/bemenu-passmenu"
                          ,(local-file (string-append %distro-directory "/dot_local/bin/passmenu")
                                       #:recursive? #t)))))

(define looking-glass-configuration-service
  (simple-service 'looking-glass-wrapper
                  home-files-service-type
                  (list `(".local/bin/looking-glass-client-wrapper"
                          ,(program-file "looking-glass-client-wrapper"
                                         #~(let ((args (cdr (command-line))))
                                             (apply execl
                                                    `(#$(file-append looking-glass-client "/bin/looking-glass-client")
                                                        "looking-glass-client"
                                                        "spice:enable" "no"
                                                        "wayland:warpSupport" "no"
                                                        "input:grabKeyboard" "no"
                                                        "win:dontUpscale" "yes"
                                                        ,@(if (file-exists? "/dev/kvmfr0")
                                                              '("-f" "/dev/kvmfr0")
                                                              '())
                                                        ,@args))))))))

(define (%workstation-home-environment)
  (home-environment
   (packages (manifest->packages (%workstation-manifest)))
   (services (list (if (file-exists?
                        (string-append %distro-directory
                                       "/wugi/home/config/openssh.scm"))
                       (service home-openssh-service-type
                                (@ (wugi home config openssh)
                                   %home-openssh-configuration))
                       (service home-openssh-service-type))

                   (service home-dbus-service-type)

                   (service home-pipewire-service-type)

                   (service home-files-service-type)

                   waybar-configuration-service

                   mako-configuration-service

                   aichat-configuration-service

                   (service home-niri-service-type)
                   niri-configuration-service

                   fuzzel-service
                   fuzzel-configuration-service

                   (service home-scream-service-type
                            (scream-configuration
                             (port 16400)))

                   (service home-alacritty-service-type)

                   gnupg-configuration-service

                   pipewire-configuration-service

                   bin-configuration-service

                   host-namespace-configuration-service

                   wl-mirror-configuration-service

                   bemenu-configuration-service

                   home-bash-service

                   (service home-bash-service-type
                            (home-bash-configuration
                             (bashrc
                              (list
                               (local-file
                                (string-append %distro-directory "/dot_bashrc"))))
                             (environment-variables
                              `(("PATH" .
                                 ,(string-append "${HOME}/bin"
                                                 ":" "${HOME}/.local/bin"
                                                 ;; ":" "$(/usr/bin/env --ignore-environment sh --norc --noprofile -c 'unset PATH; export HOME=/home/oleg; export USER=oleg; source /etc/profile; printf $PATH')"
                                                 ":" "${HOME}/go/bin"
                                                 ":" "${HOME}/.npm-global/bin"
                                                 ":" "/opt/gradle/bin"
                                                 ":" "${HOME}/perl5/bin"
                                                 ;; ":" "${HOME}/.nix-profile/lib/openjdk/bin"
                                                 ;; ":" "${HOME}/.nix-profile/bin"
                                                 ":" "$PATH"))
                                ("LC_TIME" . "en_GB.UTF-8")
                                ("LANG" . "en_US.UTF-8")

                                ("CHICKEN_REPOSITORY" . "${HOME}/.eggs/lib/chicken/8")
                                ("CHICKEN_DOC_REPOSITORY" . "${HOME}/.eggs/share/chicken-doc")

                                ("BROWSER" . "icecat")
                                ("INFOPATH" . "${HOME}/src/codeberg.org/guix/guix/doc${INFOPATH:+:}$INFOPATH")
                                ("GUILE_WARN_DEPRECATED" . "no")

                                ;; Fix mouse wheel in gtk3
                                ;; https://github.com/stumpwm/stumpwm/wiki/FAQ
                                ("GDK_CORE_DEVICE_EVENTS" . "1")

                                ("QT_QPA_PLATFORMTHEME" . "gtk2")
                                ("GUILE_LOAD_PATH" . "${HOME}/.config:${GUILE_LOAD_PATH}")
                                ("RIPGREP_CONFIG_PATH" . "${HOME}/.config/ripgrep/ripgreprc")
                                ("SSHRC_BECOME" . "yes")
                                ("GRADLE_HOME" . "/opt/gradle")
                                ("PYTHONSTARTUP" . "${HOME}/.pythonrc")
                                ("TMUXIFIER_LAYOUT_PATH" . "${HOME}/.tmuxifier-layouts")
                                ("EDITOR" . "emacsclient -nw -c")
                                ("MANWIDTH" . "80")
                                ("PERL5LIB" . "${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}")
                                ("PERL_LOCAL_LIB_ROOT" . "${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}")
                                ("PERL_MB_OPT" . "--install_base ${HOME}/perl5")
                                ("PERL_MM_OPT" . "INSTALL_BASE=${HOME}/perl5")))

                             ;; GUIX_PROFILE variable used in my custom
                             ;; .bashrc file.
                             (variables '(("GUIX_PROFILE" . "${HOME}/.guix-home/profile")))))
                   home-mime-service
                   home-direnv-service
                   home-git-service
                   home-gita-service
                   home-gdb-service
                   home-emacs-state-service
                   home-emacs-service
                   home-nano-service
                   home-inputrc-service
                   home-tmux-service
                   tmuxifier-service
                   home-top-service
                   home-nix-service
                   home-qterminal-service
                   home-gtk-service
                   home-gtkrc-service
                   home-ripgrep-service
                   home-screen-service
                   home-sbcl-service
                   home-python-service
                   home-bind-utils-service
                   ;; home-shellcheck-service
                   home-bin-service
                   home-ghci-service
                   home-groovy-service
                   home-guile-service
                   home-kodi-service
                   home-mailcap-service
                   home-mongo-service
                   home-postgresql-service
                   home-mycli-service
                   home-parallel-service
                   home-youtube-dl-service
                   home-wireplumber-config-service
                   home-mpv-service

                   looking-glass-configuration-service

                   (service home-msmtp-service-type
                            (home-msmtp-configuration
                             (default-account "gmail")
                             (extra-content "\
auth on
tls on
tls_trust_file /etc/ssl/certs/ca-certificates.crt\n")
                             (accounts
                              (list
                               (msmtp-account
                                (name "gmail")
                                (configuration
                                 (msmtp-configuration
                                  (host "smtp.gmail.com")
                                  (port 587)
                                  (user "go.wigust")
                                  (password-eval "gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/.password-store/myaccount.google.com/apppasswords/go.wigust.gpg"))))))))))))
