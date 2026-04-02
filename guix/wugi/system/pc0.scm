;; -*- mode: scheme; -*-
;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(define-module (wugi system pc0)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages video)
  #:use-module (gnu packages ssh)
  #:use-module ((gnu services) #:select (delete
                                         service-type
                                         service-extension
                                         service-value
                                         service
                                         simple-service
                                         modify-services
                                         activation-service-type))
  #:use-module (gnu services base)
  #:use-module (gnu services dns)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)
  #:use-module (gnu services monitoring)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sound)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services)
  #:use-module (wugi services containers)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system setuid)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (wugi bootloader grub)
  #:use-module (wugi config)
  #:use-module (wugi services backup)
  #:use-module (wugi services bird)
  #:use-module (wugi services kubernetes)
  #:use-module (wugi services virtualization)
  #:use-module (wugi utils)
  #:use-module (wugi utils package)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (%pc0))

(define %private-ip-address
  "192.168.0.192")

(define ns-net-guix-workstation-program-file
  (program-file "ns-net-guix-workstation"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (unless (file-exists? "/var/run/netns/guix-workstation")
                        (invoke "ip" "netns" "add" "guix-workstation")
                        (invoke "ip" "link" "add" "name" "guix0" "type" "veth" "peer" "name" "guix1")
                        (invoke "ip" "link" "set" "dev" "guix1" "netns" "guix-workstation")
                        (invoke "ip" "netns" "exec" "guix-workstation" "ip" "link" "set" "guix1" "name" "eth0")
                        (invoke "ip" "netns" "exec" "guix-workstation" "ip" "link" "set" "eth0" "up")
                        (invoke "ip" "link" "set" "guix0" "master" "br0")
                        (invoke "ip" "link" "set" "guix0" "up"))))))

(define bird-config
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/etc/bird")
      (copy-file #$(local-file (string-append %distro-directory "/dotfiles/pc0/etc/bird/bird.1.conf"))
                 "/etc/bird/bird.1.conf")
      (mkdir-p "/etc/bird/peers")
      (copy-file #$(local-file (string-append %distro-directory "/dotfiles/pc0/etc/bird/peers/nixos-hev.conf"))
                 "/etc/bird/peers/nixos-hev.conf")))

(define kresd-config
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/etc/knot-resolver")
      (copy-file #$(generate-kresd-file %private-ip-address)
                  "/etc/knot-resolver/kresd.conf")))

(define (%pc0)
  (operating-system
    (host-name "pc0")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")
    (initrd microcode-initrd)
    (kernel linux-6.18)
    (firmware (append (list linux-firmware) %base-firmware))

    ;; Use the UEFI variant of GRUB with the EFI System
    ;; Partition mounted on /boot/efi.
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader-removable)
                 (targets '("/boot/efi"))))

    (kernel-loadable-modules (list ddcci-driver-linux
                                   v4l2loopback-linux-module))

    (mapped-devices
     (list (mapped-device
            (source (uuid "a02ff162-a90f-4d01-b6a0-34ee79f95ae2"))
            (target "crypt-guix")
            (type luks-device-mapping))
           (mapped-device
            (source "vg0")
            (targets '("vg0-guixroot"))
            (type lvm-device-mapping))))

    ;; Assume the target root file system is labelled "my-root",
    ;; and the EFI System Partition has UUID 1234-ABCD.
    (file-systems (append
                   (list (file-system
                           (device "/dev/mapper/vg0-guixroot")
                           (mount-point "/")
                           (dependencies mapped-devices)
                           (type "ext4"))
                         (file-system
                           (device (uuid "25E3-69CD" 'fat))
                           (mount-point "/boot/efi")
                           (type "vfat"))
                         (file-system
                           (device "tmpfs")
                           (mount-point "/tmp")
                           (type "tmpfs")
                           (check? #f)
                           (flags '(no-dev))
                           (options "mode=1777,size=50%"))
                         (file-system
                           (device (file-system-label "guixworkstation"))
                           (mount-point "/srv/container/guix-workstation")
                           (dependencies mapped-devices)
                           (type "btrfs")))
                   %control-groups
                   %base-file-systems))

    (swap-devices
     (list
      (swap-space
       (target (file-system-label "swap"))
       (dependencies mapped-devices))))

    (kernel-arguments '("net.ifnames=0"
                        "biosdevname=0"

                        "modprobe.blacklist=pcspkr,snd_pcsp"

                        ;; Enable LUKS TRIM/DISCARD pass-through.
                        "rd.luks.options=discard"

                        ;; <https://pve.proxmox.com/wiki/PCI_Passthrough>
                        ;; Some Windows applications like GeForce Experience,
                        ;; Passmark Performance Test and SiSoftware Sandra can
                        ;; crash the VM.
                        "kvm.ignore_msrs=1"

                        "amdgpu.ppfeaturemask=0xffffffff"))

    ;; This is where user accounts are specified.  The "root"
    ;; account is implicit, and is initially created with the
    ;; empty password.
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

    ;; Globally-installed packages.
    (packages (append (map package-from-program-file
                           (list restic-pc0-backup
                                 restic-pc0-win11-backup
                                 restic-pc0-ntfsgames-backup))
                      %pc0-packages
                      %base-packages))

    (hosts-file
     (generate-hosts-file
      '("127.0.0.1 localhost pc0"
        "::1 localhost pc0")))

    (sudoers-file
     (plain-file
      "sudoers"
      (string-join '("Defaults:root runcwd=*"
                     "root ALL=(ALL) ALL"
                     "%wheel ALL=(ALL) ALL"
                     "oleg ALL=(ALL) NOPASSWD:ALL")
                   "\n")))

    ;; Add services to the baseline: a DHCP client and an SSH
    ;; server.  You may wish to add an NTP service here.
    (services (append (list (service syslog-service-type
                                     (syslog-configuration
                                      (extra-options '("--rcfile=/etc/syslog.conf"
                                                       "--no-forward"
                                                       "--no-unixaf"
                                                       "--no-klog"))))
                            (service openssh-service-type
                                     (openssh-configuration
                                      (openssh openssh-sans-x)
                                      (permit-root-login 'prohibit-password)))

                            (service kernel-module-loader-service-type
                                     '("amdgpu"

                                       "dm-snapshot"
                                       "dm-thin-pool"
                                       "br_netfilter" ;kube-dns
                                       ;; "drbd9"
                                       ;; "ddcci_backlight"

                                       ;; Required for Cilium CNI.
                                       "ip_tables"
                                       "xt_socket"
                                       "iptable_nat"
                                       "iptable_mangle"
                                       "iptable_raw"
                                       "iptable_filter"

                                       ;; ddc to backlight interface.
                                       "ddcci"
                                       "ddcci_backlight"))

                            (service container-service-type
                                     (container-configuration
                                      (bundle "/srv/container/guix-workstation")
                                      (requirement '(file-system-/srv/container/guix-workstation
                                                     ns-net-guix-workstation))
                                      (name "guix-workstation")
                                      (auto-start? #t)))

                            (simple-service 'ns-net-guix-workstation shepherd-root-service-type
                                            (list (shepherd-service
                                                   (provision '(ns-net-guix-workstation))
                                                   (requirement '(networking))
                                                   (start #~(make-forkexec-constructor
                                                             (list #$ns-net-guix-workstation-program-file)))
                                                   (respawn? #f)
                                                   (auto-start? #t)
                                                   (one-shot? #t))))

                            (udev-rules-service 'kvm
                                                (udev-rule
                                                 "91-kvm-custom.rules"
                                                 "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0666\"\n"))

                            (simple-service 'add-kresd-config
                                            activation-service-type
                                            kresd-config)

                            (simple-service 'add-bird-config
                                            activation-service-type
                                            bird-config)

                            (service knot-resolver-service-type
                                     (knot-resolver-configuration
                                      (kresd-config-file
                                       "/etc/knot-resolver/kresd.conf")))

                            ;; Bring eth0 up and pass it to the networking bridge.
                            (service static-networking-service-type
                                     (list
                                      (static-networking
                                       (provision '(eth0))
                                       (addresses (list
                                                   (network-address
                                                    (device "eth0")
                                                    (value "127.0.0.2/8")))))
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
                                                    (value "192.168.0.192/32"))))
                                       (name-servers '("192.168.0.192"

                                                       ;; local Docker
                                                       ;; "172.17.0.1"

                                                       ;; Google
                                                       ;; "8.8.8.8"
                                                       ;; "8.8.4.4"
                                                       )))
                                      (static-networking
                                       (provision '(networking))
                                       (requirement '(eth0 br0))
                                       (links (list
                                               (network-link
                                                (name "eth0")
                                                (arguments '((master . "br0"))))))
                                       (addresses '()))))
                            (service bird-service-type
                                     (bird-configuration
                                      (config-file
                                       (local-file
                                        (string-append
                                         %distro-directory
                                         "/dotfiles/pc0/etc/bird/bird.conf")))))

                            (service ntp-service-type))
                      (modify-services %base-services
                        (delete shepherd-system-log-service-type)
                        (guix-service-type config =>
                                           (guix-configuration
                                            (authorized-keys
                                             (let ((substitute-file
                                                    (cut string-append %distro-directory "/wugi/etc/substitutes/" <>)))
                                               (append (list (local-file (substitute-file "/guix.wugi.info.pub"))
                                                             (local-file (substitute-file "/vm1.wugi.info.pub"))
                                                             (local-file (substitute-file "/vm2.wugi.info.pub"))
                                                             (local-file (substitute-file "/mirror.brielmaier.net.pub"))
                                                             (local-file (substitute-file "/substitutes.nonguix.org.pub"))
                                                             (local-file (substitute-file "/bordeaux.guix.gnu.org.pub")))
                                                       %default-authorized-guix-keys)))
                                            (substitute-urls '("https://bordeaux.guix.gnu.org"
                                                               "https://substitutes.nonguix.org"))))
                        (sysctl-service-type _ =>
                                             (sysctl-configuration
                                              (settings (append '(("kernel.sysrq" . "1")
                                                                  ("net.bridge.bridge-nf-call-iptables" . "0")
                                                                  ;; for runc containers and libvirt virtual machines
                                                                  ("net.ipv4.conf.br0.forwarding" . "1"))
                                                                %default-sysctl-settings))))
                        (console-font-service-type
                         configuration =>
                         (map
                          (match-lambda
                            (("tty1" . f)
                             `("tty1" . ,(file-append font-terminus
                                                      "/share/consolefonts/ter-132n")))
                            (("tty2" . f)
                             `("tty2" . ,(file-append font-terminus
                                                      "/share/consolefonts/ter-132n")))
                            (("tty3" . f)
                             `("tty3" . ,(file-append font-terminus
                                                      "/share/consolefonts/ter-132n")))
                            (("tty4" . f)
                             `("tty4" . ,(file-append font-terminus
                                                      "/share/consolefonts/ter-132n")))
                            (("tty5" . f)
                             `("tty5" . ,(file-append font-terminus
                                                      "/share/consolefonts/ter-132n")))
                            (("tty6" . f)
                             `("tty6" . ,(file-append font-terminus
                                                      "/share/consolefonts/ter-132n")))
                            ((tty . font) `(,tty . ,font)))
                          configuration)))))))
