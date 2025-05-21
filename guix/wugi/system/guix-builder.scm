;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

;; guix system reconfigure --skip-checks --no-bootloader /etc/config.scm
;;
;; also make sure to update /entrypoint.sh

(define-module (wugi system guix-builder)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu system linux-container)
  #:use-module (gnu)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:export (%guix-builder))

(define (%my-operating-system)
  (define my-channels
    (include "/etc/channels.scm"))

  (operating-system
    (host-name "builder")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

    ;; This is where user accounts are specified.  The "root" account is
    ;; implicit, and is initially created with the empty password.
    (users (append (list (user-account
                          (name "oleg")
                          (comment "Oleg Pykhalov")
                          (group "users")

                          ;; Adding the account to the "wheel" group
                          ;; makes it a sudoer.  Adding it to "audio"
                          ;; and "video" allows the user to play sound
                          ;; and access the webcam.
                          (supplementary-groups '("wheel"
                                                  "audio"
                                                  "video"
                                                  "kvm"
                                                  "input"))))
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
    (packages (append (list bash-completion openssh)
                      %base-packages))

    (services (append (list
                       (service openssh-service-type
                                (openssh-configuration
                                 (openssh openssh-sans-x)
                                 (permit-root-login 'prohibit-password)
                                 (use-pam? #f) ;elogind requirement
                                 (password-authentication? #f)))
                       (service guix-publish-service-type
                                (guix-publish-configuration
                                 (host "0.0.0.0")
                                 (port 5556)
                                 (ttl (* 90 24 3600))))
                       (elogind-service))
                      (modify-services
                          (modify-services %base-services
                            (guix-service-type config =>
                                               (guix-configuration
                                                (channels my-channels)
                                                (guix (guix-for-channels my-channels))
                                                (authorized-keys (append (list (local-file "/etc/substitutes/guix.wugi.info.pub")
                                                                               (local-file "/etc/substitutes/vm1.wugi.info.pub")
                                                                               (local-file "/etc/substitutes/vm2.wugi.info.pub")
                                                                               (local-file "/etc/substitutes/mirror.brielmaier.net.pub")
                                                                               (local-file "/etc/substitutes/substitutes.nonguix.org.pub")
                                                                               (local-file "/etc/substitutes/bordeaux.guix.gnu.org.pub"))
                                                                         %default-authorized-guix-keys))
                                                (substitute-urls '("https://bordeaux.guix.gnu.org"
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
                                           "\n")))))

(containerized-operating-system %my-operating-system
                                (cons %store-mapping '())
                                #:shared-network? #t)
