;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

;; guix system image -t docker --network ./docker-image-workstation.scm
;; skopeo copy docker-archive:/gnu/store/…-tor-docker-pack.tar.gz docker-daemon:example.org:5000/tor:latest --insecure-policy
;; docker run --network=host --security-opt seccomp=unconfined --detach --name tor --network=host example.org:5000/tor
;; docker exec --detach tor /gnu/store/…-tor-0.4.6.10/bin/tor -f /gnu/store/…-torrc

(use-modules (gnu packages admin)
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
             (srfi srfi-1))

(use-package-modules gnupg linux pulseaudio ssh terminals virtualization wm)
(use-service-modules avahi base desktop docker dbus shepherd)

(use-modules (services desktop)
             (services docker))

(use-modules (nongnu packages chrome)
             (nongnu packages mozilla))

(define oleg-home
  (load "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/home/workstation.scm"))

(define container-mingetty-service-type
  (service-type (name 'mingetty)
                (extensions (list (service-extension shepherd-root-service-type
                                                     (@@ (gnu services base) mingetty-shepherd-service))))
                (description
                 "Provide console login using the @command{mingetty}
program.")))

(define my-channels
  (include "/home/oleg/.local/share/chezmoi/dotfiles/channels-current-guix-image-workstation.scm"))

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
                                                "audio" "video"
                                                "docker"))
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
                                    (policy-file (local-file "etc/containers/policy.json"))))
                          (service containerd-service-type)
                          (service docker-service-type))
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
