;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

;; guix system image -t docker --network ./docker-image-workstation.scm
;; skopeo copy docker-archive:/gnu/store/…-tor-docker-pack.tar.gz docker-daemon:example.org:5000/tor:latest --insecure-policy
;; docker run --network=host --security-opt seccomp=unconfined --detach --name tor --network=host example.org:5000/tor
;; docker exec --detach tor /gnu/store/…-tor-0.4.6.10/bin/tor -f /gnu/store/…-torrc

(use-modules (gnu packages)
             (gnu services)
             (gnu)
             (guix channels)
             (guix gexp)
             (guix inferior)
             (guix packages)
             (guix profiles)
             (guix ui)
             (srfi srfi-1))

(use-package-modules package-management ssh)
(use-service-modules base guix shepherd ssh)

(define my-channels
  (include "/home/oleg/.local/share/chezmoi/dotfiles/channels-current-guix-image-builder.scm"))

(operating-system
  (host-name "builder")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  ;; This is where user accounts are specified.  The "root" account is
  ;; implicit, and is initially created with the empty password.
  (users (append (list (user-account (inherit %root-account)
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

  (services (append (list
                     (service openssh-service-type
                              (openssh-configuration
                               (openssh openssh-sans-x))))
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
                                           "%wheel ALL=(ALL) ALL")
                                         "\n"))))
