;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

;; guix system image -t docker --network ./docker-image-workstation.scm
;; skopeo copy docker-archive:/gnu/store/…-tor-docker-pack.tar.gz docker-daemon:example.org:5000/tor:latest --insecure-policy
;; docker run --network=host --security-opt seccomp=unconfined --detach --name tor --network=host example.org:5000/tor
;; docker exec --detach tor /gnu/store/…-tor-0.4.6.10/bin/tor -f /gnu/store/…-torrc

(use-modules (gnu home services desktop)
             (gnu home services shells)
             (gnu home services shells)
             (gnu home services sound)
             (gnu home services)
             (gnu home)
             (gnu packages admin)
             (gnu packages)
             (gnu packages package-management)
             (gnu services guix)
             (gnu services shepherd)
             (gnu services)
             (gnu)
             (guix channels)
             (guix gexp)
             (guix packages)
             (guix profiles)
             (guix inferior)
             (guix ui)
             (srfi srfi-1))

(use-package-modules gnupg linux pulseaudio ssh terminals virtualization wm)
(use-service-modules avahi base desktop dbus shepherd)

(use-modules (services desktop)
             (services docker)
             (home config)
             (home services audio)
             (home services databases)
             (home services desktop)
             (home services dns)
             (home services emacs)
             (home services gdb)
             (home services gnupg)
             (home services groovy)
             (home services gtk)
             (home services guile)
             (home services haskell-apps)
             (home services kodi)
             (home services linux)
             (home services lisp)
             (home services mail)
             (home services mime)
             (home services nano)
             (home services nix)
             (home services python)
             (home services rust-apps)
             (home services shell)
             (home services terminals)
             (home services tmux)
             (home services version-control)
             (home services video)
             (home services web))

(use-modules (nongnu packages chrome)
             (nongnu packages mozilla))

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

(define oleg-home
  (home-environment
   (packages (packages-from-manifest "/home/oleg/.local/share/chezmoi/dotfiles/manifests/pc0.scm"))
   (services (list (service home-dbus-service-type)
                   (service home-pipewire-service-type)
                   (simple-service 'test-config
                        home-xdg-configuration-files-service-type
                        (list `("test.conf"
                                ,(plain-file "tmp-file.txt"
                                             "the content of
                                               ~/.config/test.conf"))))
                   (service home-files-service-type)
                   (simple-service 'sway-config
                                   home-files-service-type
                                   (append (list `(".xkb/symbols/custom" ,(local-file (string-append %project-directory "/dot_xkb/symbols/custom"))))
                                           (list `("bin/move" ,(program-file "sway-move"
                                                                             #~(let ((args (cdr (command-line))))
                                                                                 (execl #$(file-append sway "/bin/swaymsg") "swaymsg" "move" "workspace" (string-join args)))))
                                                 `("bin/workspace" ,(program-file "sway-workspace"
                                                                                  #~(let ((args (cdr (command-line))))
                                                                                      (execl #$(file-append sway "/bin/swaymsg") "swaymsg" "workspace" (string-join args)))))
                                                 `("bin/workspaces" ,(local-file (string-append %project-directory "/dot_local/bin/executable_sway-workspaces")
                                                                                 #:recursive? #t)))))
                   (service home-sway-service-type)
                   (service home-scream-service-type
                            (scream-configuration
                             (port 16400)))
                   (simple-service 'gnupg-config
                                   home-files-service-type
                                   (list `(".gnupg/gpg.conf"
                                           ,(local-file (string-append %project-directory "/private_dot_gnupg/gpg.conf")))
                                         `(".gnupg/gpg-agent.conf"
                                           ,(mixed-text-file "gpg-agent.conf" "\
no-grab
pinentry-program " (file-append pinentry "/bin/pinentry") "
pinentry-timeout 5
default-cache-ttl 172800
default-cache-ttl-ssh 172800
max-cache-ttl 172800
max-cache-ttl-ssh 172800
allow-preset-passphrase"))))

                   (simple-service 'bin-manual-scripts
                                   home-files-service-type
                                   (list `("bin/manual-scripts-root-02-net.sh"
                                           ,(local-file (string-append %project-directory "/dotfiles/run/guix-workstation/03-net.sh")
                                                        #:recursive? #t))
                                         `("bin/manual-scripts-root-03-firefox-twitch-namespace.sh"
                                           ,(local-file (string-append %project-directory "/dotfiles/run/guix-workstation/04-firefox-twitch-namespace.sh")
                                                        #:recursive? #t))
                                         `("bin/manual-scripts-root-04-mjru-net"
                                           ,(program-file "manual-scripts-root-04-mjru-net"
                                                          #~(let ((gateway "192.168.0.145"))
                                                              (for-each (lambda (network)
                                                                          (system* #$(file-append iproute "/sbin/ip") "route" "add" network "via" gateway))
                                                                        '("172.16.103.0/24"
                                                                          "78.108.80.0/24"
                                                                          "78.108.88.0/24")))))
                                         `("bin/manual-scripts-oleg-01-ssh.sh"
                                           ,(local-file (string-append %project-directory "/dotfiles/run/guix-workstation/02-ssh.sh")
                                                        #:recursive? #t))
                                         `("bin/manual-scripts-oleg-02-gnupg.sh"
                                           ,(local-file (string-append %project-directory "/dotfiles/run/guix-workstation/05-gnupg.sh")
                                                        #:recursive? #t))))
                   (simple-service 'bin-namespace-host
                                   home-files-service-type
                                   (list `("bin/namespace-host"
                                           ,(program-file "namespace-host"
                                                          #~(execl #$(file-append openssh "/bin/ssh")
                                                                   "ssh" "-t" "192.168.0.192" "tmux")))))
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
                                          "HEADLESS-8")))
                   (simple-service 'bin-bemenu-scripts
                                   home-files-service-type
                                   (list `("bin/bemenu-passmenu"
                                           ,(local-file (string-append %project-directory "/dot_local/bin/executable_passmenu")
                                                        #:recursive? #t))))
                   home-bash-service
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
                   home-top-service
                   home-nix-service
                   home-alacritty-service
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
                                                                         ,@args)))))))))))

(define container-mingetty-service-type
  (service-type (name 'mingetty)
                (extensions (list (service-extension shepherd-root-service-type
                                                     (@@ (gnu services base) mingetty-shepherd-service))))
                (description
                 "Provide console login using the @command{mingetty}
program.")))

(define my-channels
  (include "/home/oleg/.local/share/chezmoi/dotfiles/channels-guix-guixsd.scm"))

(operating-system
  (host-name "workstation")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  ;; This is where user accounts are specified.  The "root" account is
  ;; implicit, and is initially created with the empty password.
  (users (append (list (user-account
                        (name "oleg")
                        (comment "Oleg Pykhalov")
                        (group "users")
                        (supplementary-groups '("wheel"
                                                "audio" "video"))
                        (password (crypt "oleg" "$6$abc")))
                       (user-account (inherit %root-account)
                                     (password (crypt "root" "$6$abc"))))
                 %base-user-accounts))

  ;; Because the system will run in a Docker container, we may omit many
  ;; things that would normally be required in an operating system
  ;; configuration file.  These things include:
  ;;
  ;;   * bootloader
  ;;   * file-systems
  ;;   * services such as mingetty, udevd, slim, networking, dhcp
  ;;
  ;; Either these things are simply not required, or Docker provides
  ;; similar services for us.

  ;; This will be ignored.
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("does-not-matter"))))
  ;; This will be ignored, too.
  (file-systems (list (file-system
                        (device "does-not-matter")
                        (mount-point "/")
                        (type "does-not-matter"))))

  ;; Globally-installed packages.
  (packages (append (list openssh)
                    %base-packages))

  (services (append (list (service guix-home-service-type
                                   `(("oleg" ,oleg-home)))
                          (dbus-service)
                          (elogind-service)
                          seatd-service
                          (service container-mingetty-service-type
                                   (mingetty-configuration (tty "tty8")))
                          (service avahi-service-type)
                          (simple-service 'host-container-guix shepherd-root-service-type
                                          (list
                                           (shepherd-service
                                            (provision '(host-container-guix))
                                            (auto-start? #t)
                                            (one-shot? #t)
                                            (documentation "Provision Guix container.")
                                            (requirement '())
                                            (start #~(make-forkexec-constructor
                                                      (list #$(file-append shepherd "/bin/herd")
                                                            "--socket=/mnt/guix/var/run/shepherd/socket"
                                                            "start" "container-guix")))
                                            (respawn? #f))))
                          (service skopeo-service-type
                                   (skopeo-configuration
                                    (policy-file (local-file "etc/containers/policy.json")))))
                    (modify-services
                        (modify-services %base-services
                          (guix-service-type config =>
                                             (guix-configuration
                                              (channels my-channels)
                                              (guix (guix-for-channels my-channels))
                                              (authorized-keys (append (list (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/guix.wugi.info.pub")
                                                                             (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm1.wugi.info.pub")
                                                                             (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm2.wugi.info.pub")
                                                                             (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/mirror.brielmaier.net.pub")
                                                                             (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/substitutes.nonguix.org.pub")
                                                                             (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/bordeaux.guix.gnu.org.pub"))
                                                                       %default-authorized-guix-keys))
                                              (substitute-urls '("https://guix.wugi.info"
                                                                 "https://bordeaux.guix.gnu.org"
                                                                 "https://substitutes.nonguix.org")))))
                      (syslog-service-type config =>
                                           (syslog-configuration
                                            (extra-options '("--rcfile=/etc/syslog.conf"
                                                             "--no-forward"
                                                             "--no-unixaf"
                                                             "--no-klog")))))))

  (sudoers-file (plain-file "sudoers"
                            (string-join `("Defaults:root runcwd=*"
                                           "root ALL=(ALL) ALL"
                                           "%wheel ALL=(ALL) ALL"
                                           "oleg ALL=(ALL) NOPASSWD:ALL")
                                         "\n"))))
