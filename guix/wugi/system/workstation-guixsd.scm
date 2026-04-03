;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

(define-module (wugi system workstation-guixsd)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages wm)
  #:use-module ((gnu services) #:select (service-type
                                         service-extension
                                         service
                                         simple-service
                                         modify-services))
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system linux-container)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (srfi srfi-1)
  #:use-module (wugi config)
  #:use-module (wugi etc guix channels workstation)
  #:use-module (wugi services desktop)
  #:use-module (wugi services docker)
  #:use-module (wugi services openvpn)
  #:use-module (wugi utils)
  #:export (%workstation-guixsd))

(define bird-config
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/etc/bird")
      (copy-file #$(local-file (string-append %distro-directory "/dotfiles/guixsd-guix-workstation/etc/bird/bird.1.conf"))
                 "/etc/bird/bird.1.conf")
      (mkdir-p "/etc/bird/peers")
      (copy-file #$(local-file (string-append %distro-directory "/dotfiles/guixsd-guix-workstation/etc/bird/peers/nixos-hev.conf"))
                 "/etc/bird/peers/nixos-hev.conf")
      (copy-file #$(local-file (string-append %distro-directory "/dotfiles/guixsd-guix-workstation/etc/bird/peers/nixos-dante.conf"))
                 "/etc/bird/peers/nixos-dante.conf")
      (copy-file #$(local-file (string-append %distro-directory "/dotfiles/guixsd-guix-workstation/etc/bird/peers/vm1.conf"))
                 "/etc/bird/peers/vm1.conf")))

(define (%workstation-guixsd)
  (define container-mingetty-service-type
    (service-type (name 'mingetty)
                  (extensions (list (service-extension shepherd-root-service-type
                                                       (@@ (gnu services base) mingetty-shepherd-service))))
                  (description
                   "Provide console login using the @command{mingetty}
program.")))

  (define %my-operating-system
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
                            (supplementary-groups '("wheel" "audio" "video"))
                            (password (crypt "oleg" "NmhJoj")))
                           (user-account (inherit %root-account)
                                         (password (crypt "root" "uUxBgD"))))
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

      (services (append (list (service dbus-root-service-type)
                              (service elogind-service-type)
                              seatd-service
                              (service greetd-service-type
                                       (greetd-configuration
                                        (terminals
                                         (list
                                          (greetd-terminal-configuration
                                           (terminal-vt "8")
                                           (terminal-switch #t)
                                           (default-session-user "oleg"))))))
                              (service avahi-service-type)
                              (service skopeo-service-type
                                       (skopeo-configuration
                                        (policy-file
                                         (local-file
                                          (string-append
                                           %distro-directory
                                           "/wugi/system/etc/containers/policy.json")))))
                              (service containerd-service-type)
                              (service syslog-service-type
                                       (syslog-configuration
                                        (extra-options '("--rcfile=/etc/syslog.conf"
                                                         "--no-forward"
                                                         "--no-unixaf"
                                                         "--no-klog"))))
                              (service openvpn-service-type %openvpn-configuration-majordomo.ru)
                              (service openvpn-service-type %openvpn-configuration-wugi.info)

                              (simple-service 'add-bird-config
                                              activation-service-type
                                              bird-config)
                              (service bird-service-type
                                       (bird-configuration
                                        (config-file
                                         (local-file
                                          (string-append
                                           %distro-directory
                                           "/dotfiles/guixsd-guix-workstation/etc/bird/bird.conf"))))))
                        (modify-services %base-services
                          (guix-service-type config =>
                                             (guix-configuration
                                              (channels %channels-workstation)
                                              ;; (guix (guix-for-channels %channels-workstation))
                                              (authorized-keys
                                               (append
                                                (map (lambda (file-name)
                                                       (local-file
                                                        (string-append %distro-directory "/wugi/etc/substitutes/" file-name)))
                                                     '("bordeaux.guix.gnu.org.pub"
                                                       "guix.wugi.info.pub"
                                                       "mirror.brielmaier.net.pub"
                                                       "substitutes.nonguix.org.pub"
                                                       "vm1.wugi.info.pub"
                                                       "vm2.wugi.info.pub"
                                                       "guix-builder.pub"))
                                                %default-authorized-guix-keys))
                                              (substitute-urls '("http://runc-kube1-guix-builder.guix:5556"
                                                                 "https://bordeaux.guix.gnu.org"
                                                                 "https://substitutes.nonguix.org"
                                                                 "http://ci.guix.trop.in"))))
                          (delete shepherd-system-log-service-type)
                          (delete console-font-service-type)
                          (delete mingetty-service-type)
                          (delete agetty-service-type))))

      (sudoers-file (plain-file "sudoers"
                                (string-join `("Defaults:root runcwd=*"
                                               "root ALL=(ALL) ALL"
                                               "%wheel ALL=(ALL) ALL"
                                               "oleg ALL=(ALL) NOPASSWD:ALL")
                                             "\n")))))

  (define %my-containerized-operating-system
    (containerized-operating-system %my-operating-system
                                    (cons %store-mapping '())))

  (operating-system
    (inherit %my-containerized-operating-system)
    (kernel linux-libre)
    (services
     (append
      (list (service static-networking-service-type
                     (list
                      (static-networking
                       (provision '(eth0))
                       (addresses (list
                                   (network-address
                                    (device "eth0")
                                    (value "127.0.0.1/8")))))
                      (static-networking
                       (provision '(br0))
                       (requirement '(eth0))
                       (links (list
                               (network-link
                                (name "br0")
                                (type 'bridge)
                                (arguments '()))))
                       (addresses (list
                                   (network-address
                                    (device "br0")
                                    (value "192.168.0.191/32"))))
                       (name-servers '("192.168.0.144")))
                      (static-networking
                       (provision '(networking))
                       (requirement '(eth0 br0))
                       (links (list
                               (network-link
                                (name "eth0")
                                (arguments '((master . "br0"))))))
                       (addresses '())))))
      (modify-services (operating-system-user-services %my-containerized-operating-system)
        (guix-service-type
         config =>
         (guix-configuration
          (inherit config)
          (chroot? #t))))))))
