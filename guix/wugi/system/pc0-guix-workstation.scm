;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

(define-module (wugi system pc0-guix-workstation)
  #:use-module (wugi services desktop)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages file)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sound)
  #:use-module (gnu services web)
  #:use-module (gnu system linux-container)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels docker-image)
  #:use-module (wugi config)
  #:use-module (wugi services bird)
  #:use-module (wugi services containers)
  #:use-module (wugi utils)
  #:use-module (wugi utils package)
  #:export (%pc0-guix-workstation))

(define ns-net-nixos-majordomo-program-file
  (program-file "ns-net-nixos-majordomo"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (unless (file-exists? "/var/run/netns/nixos-majordomo")
                        (invoke "ip" "netns" "add" "nixos-majordomo")
                        (invoke "ip" "link" "add" "name" "nixos2" "type" "veth" "peer" "name" "nixos3")
                        (invoke "ip" "link" "set" "dev" "nixos3" "netns" "nixos-majordomo")
                        (invoke "ip" "netns" "exec" "nixos-majordomo" "ip" "link" "set" "nixos3" "name" "eth0")
                        (invoke "ip" "netns" "exec" "nixos-majordomo" "ip" "link" "set" "eth0" "up")
                        (invoke "ip" "link" "set" "nixos2" "master" "br0")
                        (invoke "ip" "link" "set" "nixos2" "up")
                        (invoke "ip" "netns" "exec" "nixos-majordomo" "ip" "addr" "add" "192.168.0.197/32" "dev" "eth0"))))))

(define ns-net-nixos-workstation-program-file
  (program-file "ns-net-nixos-workstation"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (unless (file-exists? "/var/run/netns/nixos-workstation")
                        (invoke "ip" "netns" "add" "nixos-workstation")
                        (invoke "ip" "link" "add" "name" "nixos0" "type" "veth" "peer" "name" "nixos1")
                        (invoke "ip" "link" "set" "dev" "nixos1" "netns" "nixos-workstation")
                        (invoke "ip" "netns" "exec" "nixos-workstation" "ip" "link" "set" "nixos1" "name" "eth0")
                        (invoke "ip" "netns" "exec" "nixos-workstation" "ip" "link" "set" "eth0" "up")
                        (invoke "ip" "link" "set" "nixos0" "master" "br0")
                        (invoke "ip" "link" "set" "nixos0" "up")
                        (invoke "ip" "netns" "exec" "nixos-workstation" "ip" "addr" "add" "192.168.0.195/32" "dev" "eth0"))))))

(define ns-net-nixos-zapret-program-file
  (program-file "ns-net-nixos-zapret"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (unless (file-exists? "/var/run/netns/nixos-zapret")
                        (invoke "ip" "netns" "add" "nixos-zapret")
                        (invoke "ip" "link" "add" "name" "nixos4" "type" "veth" "peer" "name" "nixos5")
                        (invoke "ip" "link" "set" "dev" "nixos5" "netns" "nixos-zapret")
                        (invoke "ip" "netns" "exec" "nixos-zapret" "ip" "link" "set" "nixos5" "name" "eth0")
                        (invoke "ip" "netns" "exec" "nixos-zapret" "ip" "link" "set" "eth0" "up")
                        (invoke "ip" "link" "set" "nixos4" "master" "br0")
                        (invoke "ip" "link" "set" "nixos4" "up")
                        (invoke "ip" "netns" "exec" "nixos-zapret" "ip" "addr" "add" "192.168.0.175/32" "dev" "eth0"))))))

(define ns-net-nixos-tor-program-file
  (program-file "ns-net-nixos-tor"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-tor")
                      (invoke "ip" "link" "add" "name" "nixos6" "type" "veth" "peer" "name" "nixos7")
                      (invoke "ip" "link" "set" "dev" "nixos7" "netns" "nixos-tor")
                      (invoke "ip" "netns" "exec" "nixos-tor" "ip" "link" "set" "nixos7" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-tor" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos6" "master" "br0")
                      (invoke "ip" "link" "set" "nixos6" "up")
                      (invoke "ip" "netns" "exec" "nixos-tor" "ip" "addr" "add" "192.168.0.190/32" "dev" "eth0")))))

(define ns-net-nixos-antifilter-program-file
  (program-file "ns-net-nixos-antifilter"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-antifilter")
                      (invoke "ip" "link" "add" "name" "nixos8" "type" "veth" "peer" "name" "nixos9")
                      (invoke "ip" "link" "set" "dev" "nixos9" "netns" "nixos-antifilter")
                      (invoke "ip" "netns" "exec" "nixos-antifilter" "ip" "link" "set" "nixos9" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-antifilter" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos8" "master" "br0")
                      (invoke "ip" "link" "set" "nixos8" "up")
                      (invoke "ip" "netns" "exec" "nixos-antifilter" "ip" "addr" "add" "192.168.0.180/32" "dev" "eth0")))))

(define ns-net-nixos-gw-program-file
  (program-file "ns-net-nixos-gw"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-gw")
                      (invoke "ip" "link" "add" "name" "nixos10" "type" "veth" "peer" "name" "nixos11")
                      (invoke "ip" "link" "set" "dev" "nixos11" "netns" "nixos-gw")
                      (invoke "ip" "netns" "exec" "nixos-gw" "ip" "link" "set" "nixos11" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-gw" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos10" "master" "br0")
                      (invoke "ip" "link" "set" "nixos10" "up")
                      (invoke "ip" "netns" "exec" "nixos-gw" "ip" "addr" "add" "192.168.0.170/32" "dev" "eth0")))))

(define ns-net-nixos-wan-program-file
  (program-file "ns-net-nixos-wan"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-wan")
                      (invoke "ip" "link" "add" "name" "nixos12" "type" "veth" "peer" "name" "nixos13")
                      (invoke "ip" "link" "set" "dev" "nixos13" "netns" "nixos-wan")
                      (invoke "ip" "netns" "exec" "nixos-wan" "ip" "link" "set" "nixos13" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-wan" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos12" "master" "br0")
                      (invoke "ip" "link" "set" "nixos12" "up")
                      (invoke "ip" "netns" "exec" "nixos-wan" "ip" "addr" "add" "192.168.0.160/32" "dev" "eth0")))))

(define ns-net-nixos-bview-program-file
  (program-file "ns-net-nixos-bview"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-bview")
                      (invoke "ip" "link" "add" "name" "nixos14" "type" "veth" "peer" "name" "nixos15")
                      (invoke "ip" "link" "set" "dev" "nixos15" "netns" "nixos-bview")
                      (invoke "ip" "netns" "exec" "nixos-bview" "ip" "link" "set" "nixos15" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-bview" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos14" "master" "br0")
                      (invoke "ip" "link" "set" "nixos14" "up")
                      (invoke "ip" "netns" "exec" "nixos-bview" "ip" "addr" "add" "192.168.0.150/32" "dev" "eth0")))))

(define ns-net-nixos-awg-program-file
  (program-file "ns-net-nixos-awg"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-awg")
                      (invoke "ip" "link" "add" "name" "nixos16" "type" "veth" "peer" "name" "nixos17")
                      (invoke "ip" "link" "set" "dev" "nixos17" "netns" "nixos-awg")
                      (invoke "ip" "netns" "exec" "nixos-awg" "ip" "link" "set" "nixos17" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-awg" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos16" "master" "br0")
                      (invoke "ip" "link" "set" "nixos16" "up")
                      (invoke "ip" "netns" "exec" "nixos-awg" "ip" "addr" "add" "192.168.0.130/32" "dev" "eth0")))))

(define ns-net-nixos-ws-program-file
  (program-file "ns-net-nixos-ws"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-ws")
                      (invoke "ip" "link" "add" "name" "nixos18" "type" "veth" "peer" "name" "nixos19")
                      (invoke "ip" "link" "set" "dev" "nixos19" "netns" "nixos-ws")
                      (invoke "ip" "netns" "exec" "nixos-ws" "ip" "link" "set" "nixos19" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-ws" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos18" "master" "br0")
                      (invoke "ip" "link" "set" "nixos18" "up")
                      (invoke "ip" "netns" "exec" "nixos-ws" "ip" "addr" "add" "192.168.0.120/32" "dev" "eth0")))))

(define ns-net-nixos-dante-program-file
  (program-file "ns-net-nixos-dante"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-dante")
                      (invoke "ip" "link" "add" "name" "nixos20" "type" "veth" "peer" "name" "nixos21")
                      (invoke "ip" "link" "set" "dev" "nixos21" "netns" "nixos-dante")
                      (invoke "ip" "netns" "exec" "nixos-dante" "ip" "link" "set" "nixos21" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-dante" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos20" "master" "br0")
                      (invoke "ip" "link" "set" "nixos20" "up")
                      (invoke "ip" "netns" "exec" "nixos-dante" "ip" "addr" "add" "192.168.0.110/32" "dev" "eth0")))))

(define ns-net-nixos-hev-program-file
  (program-file "ns-net-nixos-hev"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-hev")
                      (invoke "ip" "link" "add" "name" "nixos22" "type" "veth" "peer" "name" "nixos23")
                      (invoke "ip" "link" "set" "dev" "nixos23" "netns" "nixos-hev")
                      (invoke "ip" "netns" "exec" "nixos-hev" "ip" "link" "set" "nixos23" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-hev" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos22" "master" "br0")
                      (invoke "ip" "link" "set" "nixos22" "up")
                      (invoke "ip" "netns" "exec" "nixos-hev" "ip" "addr" "add" "192.168.0.115/32" "dev" "eth0")))))

(define ns-net-nixos-kube103-program-file
  (program-file "ns-net-nixos-kube103"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-kube103")
                      (invoke "ip" "link" "add" "name" "nixos24" "type" "veth" "peer" "name" "nixos25")
                      (invoke "ip" "link" "set" "dev" "nixos25" "netns" "nixos-kube103")
                      (invoke "ip" "netns" "exec" "nixos-kube103" "ip" "link" "set" "nixos25" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-kube103" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos24" "master" "br0")
                      (invoke "ip" "link" "set" "nixos24" "up")
                      (invoke "ip" "netns" "exec" "nixos-kube103" "ip" "addr" "add" "192.168.0.104/32" "dev" "eth0")))))

(define ns-net-fedora-program-file
  (program-file "ns-net-fedora"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (unless (file-exists? "/var/run/netns/fedora")
                        (invoke "ip" "netns" "add" "fedora")
                        (invoke "ip" "link" "add" "name" "fedora0" "type" "veth" "peer" "name" "fedora1")
                        (invoke "ip" "link" "set" "dev" "fedora1" "netns" "fedora")
                        (invoke "ip" "netns" "exec" "fedora" "ip" "link" "set" "fedora1" "name" "eth0")
                        (invoke "ip" "netns" "exec" "fedora" "ip" "link" "set" "eth0" "up")
                        (invoke "ip" "link" "set" "fedora0" "master" "br0")
                        (invoke "ip" "link" "set" "fedora0" "up")
                        (invoke "ip" "netns" "exec" "fedora" "ip" "link" "add" "name" "br0" "type" "bridge")
                        (invoke "ip" "netns" "exec" "fedora" "ip" "link" "set" "dev" "br0" "up")
                        (invoke "ip" "netns" "exec" "fedora" "ip" "link" "set" "eth0" "master" "br0")
                        (invoke "ip" "netns" "exec" "fedora" "ip" "addr" "add" "192.168.0.155/32" "dev" "br0"))))))

(define ns-net-guix-rde-program-file
  (program-file "ns-net-guix-rde"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (unless (file-exists? "/var/run/netns/guix-rde")
                        (invoke "ip" "netns" "add" "guix-rde")
                        (invoke "ip" "link" "add" "name" "guix2" "type" "veth" "peer" "name" "guix3")
                        (invoke "ip" "link" "set" "dev" "guix3" "netns" "guix-rde")
                        (invoke "ip" "netns" "exec" "guix-rde" "ip" "link" "set" "guix3" "name" "eth0")
                        (invoke "ip" "netns" "exec" "guix-rde" "ip" "link" "set" "eth0" "up")
                        (invoke "ip" "link" "set" "guix2" "master" "br0")
                        (invoke "ip" "link" "set" "guix2" "up")
                        (invoke "ip" "netns" "exec" "guix-rde" "ip" "addr" "add" "192.168.0.193/32" "dev" "eth0"))))))

(define ns-net-guix-nanokvm-program-file
  (program-file "ns-net-guix-nanokvm"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (unless (file-exists? "/var/run/netns/guix-nanokvm")
                        (invoke "ip" "netns" "add" "guix-nanokvm")
                        (invoke "ip" "link" "add" "name" "guix4" "type" "veth" "peer" "name" "guix5")
                        (invoke "ip" "link" "set" "dev" "guix5" "netns" "guix-nanokvm")
                        (invoke "ip" "netns" "exec" "guix-nanokvm" "ip" "link" "set" "guix5" "name" "eth0")
                        (invoke "ip" "netns" "exec" "guix-nanokvm" "ip" "link" "set" "eth0" "up")
                        (invoke "ip" "link" "set" "guix4" "master" "br0")
                        (invoke "ip" "link" "set" "guix4" "up")
                        (invoke "ip" "netns" "exec" "guix-nanokvm" "ip" "addr" "add" "192.168.0.198/32" "dev" "eth0"))))))

(define bird-config
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/etc/bird")
      (copy-file #$(local-file (string-append %distro-directory "/dotfiles/pc0-guix-workstation/etc/bird/bird.1.conf"))
                 "/etc/bird/bird.1.conf")
      (mkdir-p "/etc/bird/peers")
      (copy-file #$(local-file (string-append %distro-directory "/dotfiles/pc0-guix-workstation/etc/bird/peers/nixos-hev.conf"))
                 "/etc/bird/peers/nixos-hev.conf")))

(define (%pc0-guix-workstation)
  (define container-mingetty-service-type
    (service-type (name 'mingetty)
                  (extensions (list (service-extension shepherd-root-service-type
                                                       (@@ (gnu services base) mingetty-shepherd-service))))
                  (description
                   "Provide console login using the @command{mingetty}
program.")))

  (define %my-operating-system
    (operating-system
      (host-name "pc0-guix-workstation")
      (timezone "Europe/Moscow")
      (locale "en_US.utf8")

      ;; This is where user accounts are specified.  The "root" account is
      ;; implicit, and is initially created with the empty password.
      (users (append (list (user-account
                             (name "oleg")
                             (comment "Oleg Pykhalov")
                             (group "users")
                             (supplementary-groups '("wheel"
                                                     "audio"
                                                     "video"
                                                     "kvm"
                                                     "input"))
                             (password (crypt "oleg" "NmhJoj")))
                           (user-account (inherit %root-account)
                                         (password (crypt "root" "uUxBgD"))))
                     %base-user-accounts))

      ;; Globally-installed packages.
      (packages (append (list bash-completion
                              binutils
                              file
                              lvm2
                              openssh
                              glibc)
                        %base-packages))

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
                            (type "does-not-matter"))
                          (file-system
                            (device (file-system-label "fedora"))
                            (mount-point "/srv/container/fedora")
                            (type "ext4"))
                          (file-system
                            (device (file-system-label "guixnanokvm"))
                            (mount-point "/srv/container/guix-nanokvm")
                            (type "ext4"))
                          (file-system
                            (device (file-system-label "guixrde"))
                            (mount-point "/srv/container/guix-rde")
                            (type "ext4"))
                          (file-system
                            (device (file-system-label "nixosantifilter"))
                            (mount-point "/srv/container/nixos-antifilter")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixosgw"))
                            (mount-point "/srv/container/nixos-gw")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixosmajordomoroot"))
                            (mount-point "/srv/container/nixos-majordomo-root")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixosmajordomomysql"))
                            (mount-point "/srv/container/nixos-majordomo-mysql")
                            (type "ext4"))
                          (file-system
                            (device (file-system-label "nixostor"))
                            (mount-point "/srv/container/nixos-tor")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixosworkstation"))
                            (mount-point "/srv/container/nixos-workstation")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixoszapret"))
                            (mount-point "/srv/container/nixos-zapret")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixoswan"))
                            (mount-point "/srv/container/nixos-wan")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixosbview"))
                            (mount-point "/srv/container/nixos-bview")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixosawg"))
                            (mount-point "/srv/container/nixos-awg")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixosws"))
                            (mount-point "/srv/container/nixos-ws")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixosdante"))
                            (mount-point "/srv/container/nixos-dante")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixoshev"))
                            (mount-point "/srv/container/nixos-hev")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "guixws"))
                            (mount-point "/srv/container/guix-ws")
                            (type "btrfs"))
                          (file-system
                            (device (file-system-label "nixoskube103"))
                            (mount-point "/srv/container/nixos-kube103")
                            (type "ext4"))
                          (file-system
                            (device (file-system-label "steam"))
                            (mount-point "/mnt/steam")
                            (type "btrfs"))))

      ;; Guix is all you need!
      (services
       (append
        (list
         (service syslog-service-type
                  (syslog-configuration
                    (extra-options '("--rcfile=/etc/syslog.conf"
                                     "--no-forward"
                                     "--no-unixaf"
                                     "--no-klog"))))
         (service elogind-service-type)
         seatd-service
         (service dbus-root-service-type)
         (service greetd-service-type
                  (greetd-configuration
                   (terminals
                    (list
                     (greetd-terminal-configuration
                      (terminal-vt "8")
                      (terminal-switch #t)
                      (default-session-user "oleg"))))))
         (service (@ (wugi services desktop) bluetooth-service-type)
                  (bluetooth-configuration
                    (auto-enable? #t)
                    (just-works-repairing 'confirm)
                    (controller-mode 'dual)
                    (min-connection-interval 7)
                    (max-connection-interval 9)
                    (connection-latency 0)
                    (privacy 'device)))
         udev-rules-service-xbox
         (service ladspa-service-type
                  (ladspa-configuration (plugins (list swh-plugins))))
         (service avahi-service-type)

         (service guix-publish-service-type
                  (guix-publish-configuration
                   (host "0.0.0.0")
                   (port 5556)
                   (ttl (* 90 24 3600))))

         (simple-service 'add-bird-config
                         activation-service-type
                         bird-config)
         (service bird-service-type
                  (bird-configuration
                   (config-file
                    (local-file
                     (string-append
                      %distro-directory
                      "/dotfiles/pc0-guix-workstation/etc/bird/bird.conf")))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-zapret")
                   (name "nixos-zapret")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-zapret
                                  ns-net-nixos-zapret
                                  wait-for-file-/var/run/netns/nixos-zapret))
                   (auto-start? #t)
                   (wait-for-files '("/var/run/netns/nixos-zapret"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-tor")
                   (name "nixos-tor")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-tor
                                  ns-net-nixos-tor
                                  wait-for-file-/var/run/netns/nixos-tor))
                   (auto-start? #t)
                   (wait-for-files '("/var/run/netns/nixos-tor"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-gw")
                   (name "nixos-gw")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-gw
                                  ns-net-nixos-gw
                                  wait-for-file-/var/run/netns/nixos-gw))
                   (auto-start? #f)
                   (wait-for-files '("/var/run/netns/nixos-gw"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-wan")
                   (name "nixos-wan")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-wan
                                  ns-net-nixos-wan
                                  wait-for-file-/var/run/netns/nixos-wan))
                   (auto-start? #t)
                   (wait-for-files '("/var/run/netns/nixos-wan"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-antifilter")
                   (name "nixos-antifilter")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-antifilter
                                  ns-net-nixos-antifilter
                                  wait-for-file-/var/run/netns/nixos-antifilter))
                   (auto-start? #t)
                   (wait-for-files '("/var/run/netns/nixos-antifilter"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-majordomo-root")
                   (name "nixos-majordomo")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-majordomo-root
                                  file-system-/srv/container/nixos-majordomo-mysql
                                  ns-net-nixos-majordomo
                                  wait-for-file-/var/run/netns/nixos-majordomo
                                  wait-for-file-/run/user/1000/wayland-1))
                   (auto-start? #t)
                   (wait-for-files '("/var/run/netns/nixos-majordomo"
                                     "/run/user/1000/wayland-1"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-workstation")
                   (name "nixos-workstation")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-workstation
                                  file-system-/mnt/steam
                                  ns-net-nixos-workstation
                                  wait-for-file-/var/run/netns/nixos-workstation
                                  wait-for-file-/run/user/1000/wayland-1))
                   (auto-start? #t)
                   (wait-for-files '("/var/run/netns/nixos-workstation"
                                     "/run/user/1000/wayland-1"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-dante")
                   (name "nixos-dante")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-dante
                                  ns-net-nixos-dante
                                  wait-for-file-/var/run/netns/nixos-dante))
                   (auto-start? #t)
                   (wait-for-files '("/var/run/netns/nixos-dante"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-hev")
                   (name "nixos-hev")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-hev
                                  ns-net-nixos-hev
                                  wait-for-file-/var/run/netns/nixos-hev))
                   (auto-start? #t)
                   (wait-for-files '("/var/run/netns/nixos-hev"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-kube103")
                   (name "nixos-kube103")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-kube103
                                  ns-net-nixos-kube103
                                  wait-for-file-/var/run/netns/nixos-kube103))
                   (auto-start? #t)
                   (wait-for-files '("/var/run/netns/nixos-kube103"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/nixos-bview")
                   (name "nixos-bview")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/nixos-bview
                                  ns-net-nixos-bview
                                  wait-for-file-/var/run/netns/nixos-bview))
                   (auto-start? #t)
                   (wait-for-files '("/var/run/netns/nixos-bview"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/guix-nanokvm")
                   (name "guix-nanokvm")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/guix-nanokvm
                                  ns-net-guix-nanokvm
                                  wait-for-file-/var/run/netns/guix-nanokvm))
                   (auto-start? #f)
                   (wait-for-files '("/var/run/netns/guix-nanokvm"))))

         (service container-service-type
                  (container-configuration
                   (bundle "/srv/container/fedora")
                   (name "fedora")
                   (requirement '(file-system-/sys/fs/cgroup
                                  file-system-/srv/container/fedora
                                  ns-net-fedora
                                  wait-for-file-/var/run/netns/fedora))
                   (auto-start? #f)
                   (wait-for-files '("/var/run/netns/fedora"))))

         (simple-service 'ns-net-nixos-majordomo shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-majordomo))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-majordomo-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-workstation shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-workstation))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-workstation-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-zapret shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-zapret))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-zapret-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))


         (simple-service 'ns-net-nixos-tor shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-tor))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-tor-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-antifilter shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-antifilter))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-antifilter-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-gw shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-gw))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-gw-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-wan shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-wan))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-wan-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-bview shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-bview))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-bview-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-awg shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-awg))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-awg-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-ws shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-ws))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-ws-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-dante shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-dante))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-dante-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-hev shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-hev))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-hev-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-nixos-kube103 shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-nixos-kube103))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-nixos-kube103-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-fedora shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-fedora))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-fedora-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-guix-rde shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-guix-rde))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-guix-rde-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t))))

         (simple-service 'ns-net-guix-nanokvm shepherd-root-service-type
                         (list (shepherd-service
                                (provision '(ns-net-guix-nanokvm))
                                (requirement '(networking))
                                (start #~(make-forkexec-constructor
                                          (list #$ns-net-guix-nanokvm-program-file)))
                                (respawn? #f)
                                (auto-start? #t)
                                (one-shot? #t)))))

        (modify-services %base-services
          (delete console-font-service-type)
          (delete shepherd-system-log-service-type)
          (delete mingetty-service-type)
          (delete agetty-service-type)
          (guix-service-type
           config =>
           (guix-configuration
             (authorized-keys
              (append
               (map (lambda (file-name)
                      (local-file
                       (string-append %distro-directory
                                      "/wugi/etc/substitutes/" file-name)))
                    '("bordeaux.guix.gnu.org.pub"
                      "guix-builder.pub"
                      "guix.wugi.info.pub"
                      "mirror.brielmaier.net.pub"
                      "substitutes.nonguix.org.pub"
                      "vm1.wugi.info.pub"
                      "vm2.wugi.info.pub"))
               %default-authorized-guix-keys))
             (substitute-urls '("https://mirrors.sjtug.sjtu.edu.cn/guix"
                                "https://substitutes.nonguix.org")))))))

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
                                    (value "192.168.0.194/32"))))
                       (name-servers '("192.168.0.192")))
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
