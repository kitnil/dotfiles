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
  #:use-module (wugi services kubernetes)
  #:use-module (wugi services virtualization)
  #:use-module (wugi utils)
  #:use-module (wugi utils package)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (%pc0))

(define %private-ip-address
  "192.168.0.192")

(define nixos-majordomo-program-file
  (program-file "nixos-majordomo"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-majordomo")
                      (invoke "ip" "link" "add" "name" "nixos2" "type" "veth" "peer" "name" "nixos3")
                      (invoke "ip" "link" "set" "dev" "nixos3" "netns" "nixos-majordomo")
                      (invoke "ip" "netns" "exec" "nixos-majordomo" "ip" "link" "set" "nixos3" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-majordomo" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos2" "master" "br0")
                      (invoke "ip" "link" "set" "nixos2" "up")
                      (invoke "ip" "netns" "exec" "nixos-majordomo" "ip" "addr" "add" "192.168.0.197/24" "dev" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-majordomo" "ip" "route" "add" "default" "via" "192.168.0.1")))))

(define nixos-workstation-program-file
  (program-file "nixos-workstation"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-workstation")
                      (invoke "ip" "link" "add" "name" "nixos0" "type" "veth" "peer" "name" "nixos1")
                      (invoke "ip" "link" "set" "dev" "nixos1" "netns" "nixos-workstation")
                      (invoke "ip" "netns" "exec" "nixos-workstation" "ip" "link" "set" "nixos1" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-workstation" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos0" "master" "br0")
                      (invoke "ip" "link" "set" "nixos0" "up")
                      (invoke "ip" "netns" "exec" "nixos-workstation" "ip" "addr" "add" "192.168.0.195/24" "dev" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-workstation" "ip" "route" "add" "default" "via" "192.168.0.1")))))

(define nixos-tor-program-file
  (program-file "nixos-tor"
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
                      (invoke "ip" "netns" "exec" "nixos-tor" "ip" "addr" "add" "192.168.0.190/24" "dev" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-tor" "ip" "route" "add" "default" "via" "192.168.0.1")))))

(define nixos-antifilter-program-file
  (program-file "nixos-antifilter"
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
                      (invoke "ip" "netns" "exec" "nixos-antifilter" "ip" "addr" "add" "192.168.0.180/24" "dev" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-antifilter" "ip" "route" "add" "default" "via" "192.168.0.1")))))

(define nixos-zapret-program-file
  (program-file "nixos-zapret"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "nixos-zapret")
                      (invoke "ip" "link" "add" "name" "nixos4" "type" "veth" "peer" "name" "nixos5")
                      (invoke "ip" "link" "set" "dev" "nixos5" "netns" "nixos-zapret")
                      (invoke "ip" "netns" "exec" "nixos-zapret" "ip" "link" "set" "nixos5" "name" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-zapret" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "nixos4" "master" "br0")
                      (invoke "ip" "link" "set" "nixos4" "up")
                      (invoke "ip" "netns" "exec" "nixos-zapret" "ip" "addr" "add" "192.168.0.175/24" "dev" "eth0")
                      (invoke "ip" "netns" "exec" "nixos-zapret" "ip" "route" "add" "default" "via" "192.168.0.1")))))

(define fedora-program-file
  (program-file "fedora"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "fedora")
                      (invoke "ip" "link" "add" "name" "fedora0" "type" "veth" "peer" "name" "fedora1")
                      (invoke "ip" "link" "set" "dev" "fedora1" "netns" "fedora")
                      (invoke "ip" "netns" "exec" "fedora" "ip" "link" "set" "fedora1" "name" "eth0")
                      (invoke "ip" "netns" "exec" "fedora" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "fedora0" "master" "br0")
                      (invoke "ip" "link" "set" "fedora0" "up")
                      (invoke "ip" "netns" "exec" "fedora" "ip" "addr" "add" "192.168.0.155/24" "dev" "eth0")
                      (invoke "ip" "netns" "exec" "fedora" "ip" "route" "add" "default" "via" "192.168.0.1")))))

(define ns-net-guix-workstation-program-file
  (program-file "ns-net-guix-workstation"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "guix-workstation")
                      (invoke "ip" "link" "add" "name" "guix0" "type" "veth" "peer" "name" "guix1")
                      (invoke "ip" "link" "set" "dev" "guix1" "netns" "guix-workstation")
                      (invoke "ip" "netns" "exec" "guix-workstation" "ip" "link" "set" "guix1" "name" "eth0")
                      (invoke "ip" "netns" "exec" "guix-workstation" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "guix0" "master" "br0")
                      (invoke "ip" "link" "set" "guix0" "up")
                      (invoke "ip" "netns" "exec" "guix-workstation" "ip" "addr" "add" "192.168.0.194/24" "dev" "eth0")
                      (invoke "ip" "netns" "exec" "guix-workstation" "ip" "route" "add" "default" "via" "192.168.0.1")))))

(define guix-rde-program-file
  (program-file "guix-rde"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))
                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))
                      (invoke "ip" "netns" "add" "guix-rde")
                      (invoke "ip" "link" "add" "name" "guix2" "type" "veth" "peer" "name" "guix3")
                      (invoke "ip" "link" "set" "dev" "guix3" "netns" "guix-rde")
                      (invoke "ip" "netns" "exec" "guix-rde" "ip" "link" "set" "guix3" "name" "eth0")
                      (invoke "ip" "netns" "exec" "guix-rde" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "guix2" "master" "br0")
                      (invoke "ip" "link" "set" "guix2" "up")
                      (invoke "ip" "netns" "exec" "guix-rde" "ip" "addr" "add" "192.168.0.193/24" "dev" "eth0")
                      (invoke "ip" "netns" "exec" "guix-rde" "ip" "route" "add" "default" "via" "192.168.0.1")))))

(define guix-nanokvm-program-file
  (program-file "nixos-majordomo"
                (with-imported-modules (source-module-closure '((guix build utils)))
                  #~(begin
                      (use-modules (guix build utils))

                      (setenv "PATH"
                              (string-append "/run/current-system/profile/bin:"
                                             "/run/current-system/profile/sbin"))

                      (invoke "ip" "netns" "add" "guix-nanokvm")
                      (invoke "ip" "link" "add" "name" "guix4" "type" "veth" "peer" "name" "guix5")
                      (invoke "ip" "link" "set" "dev" "guix5" "netns" "guix-nanokvm")
                      (invoke "ip" "netns" "exec" "guix-nanokvm" "ip" "link" "set" "guix5" "name" "eth0")
                      (invoke "ip" "netns" "exec" "guix-nanokvm" "ip" "link" "set" "eth0" "up")
                      (invoke "ip" "link" "set" "guix4" "master" "br0")
                      (invoke "ip" "link" "set" "guix4" "up")
                      (invoke "ip" "netns" "exec" "guix-nanokvm" "ip" "addr" "add" "192.168.0.198/24" "dev" "eth0")
                      (invoke "ip" "netns" "exec" "guix-nanokvm" "ip" "route" "add" "default" "via" "192.168.0.1")))))

(define (%pc0)
  (operating-system
    (host-name "pc0")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")
    (initrd microcode-initrd)
    (initrd-modules (append '("vfio-pci") %base-initrd-modules))
    (kernel linux-6.12)
    (firmware (append (list linux-firmware) %base-firmware))

    ;; Use the UEFI variant of GRUB with the EFI System
    ;; Partition mounted on /boot/efi.
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader-removable)
                 (targets '("/boot/efi"))))

    (kernel-loadable-modules (list kvmfr-linux-module
                                   ddcci-driver-linux
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
                           (options "mode=1777,size=10%"))
                         (file-system
                           (device "hugetlbfs")
                           (mount-point "/hugepages")
                           (type "hugetlbfs"))
                         (file-system
                           (device (file-system-label "fedora"))
                           (mount-point "/srv/runc/fedora")
                           (dependencies mapped-devices)
                           (type "ext4"))
                         (file-system
                           (device (file-system-label "nixosmajordomo"))
                           (mount-point "/srv/runc/nixos-majordomo")
                           (dependencies mapped-devices)
                           (type "ext4"))
                         (file-system
                           (device (file-system-label "nixoszapret"))
                           (mount-point "/srv/runc/nixos-zapret")
                           (dependencies mapped-devices)
                           (type "btrfs"))
                         (file-system
                           (device (file-system-label "nixosworkstation"))
                           (mount-point "/srv/runc/nixos-workstation")
                           (dependencies mapped-devices)
                           (type "btrfs"))
                         (file-system
                           (device (file-system-label "guixworkstation"))
                           (mount-point "/srv/runc/guix-workstation")
                           (dependencies mapped-devices)
                           (type "btrfs"))
                         (file-system
                           (device (file-system-label "guixnanokvm"))
                           (mount-point "/srv/runc/guix-nanokvm")
                           (dependencies mapped-devices)
                           (type "ext4")))
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

                        "vfio-pci.ids=1002:7550,1002:ab40"))

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
                                                  "input"
                                                  "libvirt"))))
                   %base-user-accounts))

    ;; Globally-installed packages.
    (packages (append (map package-from-program-file
                           (list restic-pc0-backup
                                 restic-pc0-win10-backup))
                      (list dmidecode) ;required for libvirt
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

                            (simple-service 'vfio-override boot-service-type
                                            '(and (call-with-output-file "/sys/bus/pci/devices/0000:03:00.0/driver_override"
                                                    (lambda (p)
                                                      (display "vfio-pci" p)))
                                                  (call-with-output-file "/sys/bus/pci/drivers/vfio-pci/new_id"
                                                    (lambda (p)
                                                      (display "1002 7550" p)))
                                                  (call-with-output-file "/sys/bus/pci/devices/0000:03:00.1/driver_override"
                                                    (lambda (p)
                                                      (display "vfio-pci" p)))
                                                  (call-with-output-file "/sys/bus/pci/drivers/vfio-pci/new_id"
                                                    (lambda (p)
                                                      (display "1002 ab40" p)))))

                            (service kernel-module-loader-service-type
                                     '("vfio-pci"
                                       "amdgpu"

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
                                       "ddcci_backlight"

                                       "vfio_iommu_type1"))
                            (service containerd-service-type)

                            (service runc-container-service-type
                                     (runc-container-configuration
                                      (bundle "/srv/runc/guix-workstation")
                                      (requirement '(ns-net-guix-workstation
                                                     file-system-/srv/runc/fedora
                                                     file-system-/srv/runc/guix-nanokvm
                                                     file-system-/srv/runc/guix-workstation
                                                     file-system-/srv/runc/nixos-majordomo))
                                      (name "guix-workstation")
                                      (auto-start? #t)))

                            (service kubelet-service-type
                                     (kubelet-configuration
                                      (kubelet "/nix/store/lp8ch8l5dn4bcp056cpr1gfyb9i8zi54-kubernetes-1.25.4/bin/kubelet")
                                      (maintenance? #t)
                                      (arguments
                                       '("--address=192.168.0.192"
                                         "--node-ip=192.168.0.192"
                                         "--authentication-token-webhook"
                                         "--authentication-token-webhook-cache-ttl=10s"
                                         "--authorization-mode=Webhook"
                                         "--client-ca-file=/etc/kubernetes/pki/ca.pem"
                                         "--cluster-dns=10.8.255.254"
                                         "--cluster-domain=cluster.local"
                                         "--hairpin-mode=hairpin-veth"
                                         "--healthz-bind-address=127.0.0.1"
                                         "--healthz-port=10248"
                                         "--hostname-override=kube3"
                                         "--kubeconfig=/etc/kubernetes/kubeconfig"
                                         "--pod-infra-container-image=pause"
                                         "--port=10250"
                                         "--register-node=true"
                                         "--register-with-taints=unschedulable=true:NoSchedule"
                                         "--root-dir=/var/lib/kubelet"
                                         "--tls-cert-file=/etc/kubernetes/pki/kubelet-client-kube3.pem"
                                         "--tls-private-key-file=/etc/kubernetes/pki/kubelet-client-kube3-key.pem"
                                         "--container-runtime=remote"
                                         "--container-runtime-endpoint=unix:///run/containerd/containerd.sock"
                                         "--fail-swap-on=false"
                                         "--eviction-hard=nodefs.available<5Gi,nodefs.inodesFree<500000,imagefs.available<5Gi,imagefs.inodesFree<500000"
                                         "--image-gc-high-threshold=95"
                                         "--image-gc-low-threshold=90"
                                         "--pod-manifest-path=/etc/kubernetes/manifests"
                                         "--max-pods=200"))
                                      (drbd? #f)
                                      (hpvolumes? #f)
                                      (cilium? #t)
                                      (flux? #t)
                                      (kubevirt? #t)))

                            (service libvirt-service-type
                                     (libvirt-configuration
                                      ;; XXX: Specify listen-addr after adding networking requirement.
                                      ;;
                                      (listen-addr "192.168.0.192")
                                      (listen-tcp? #t)
                                      (auth-tcp "none")
                                      ;; (requirement '(networking))
                                      ))
                            (simple-service 'libvirt-qemu-config activation-service-type
                                            #~(begin
                                                (when (file-exists? "/etc/libvirt")
                                                  (with-output-to-file "/etc/libvirt/qemu.conf"
                                                    (lambda ()
                                                      (display "\
user = \"oleg\"

nvram = [
  \"/home/oleg/.nix-profile/FV/OVMF_CODE.fd:/home/oleg/.nix-profile/FV/OVMF_VARS.fd\"
]

namespaces = [ ]

cgroup_device_acl = [
   \"/dev/null\", \"/dev/full\", \"/dev/zero\",
   \"/dev/random\", \"/dev/urandom\",
   \"/dev/ptmx\", \"/dev/kvm\",
   \"/dev/vfio/vfio\",
   \"/dev/kvmfr0\"
]
"))))))

                            (simple-service 'nixos-majordomo shepherd-root-service-type
                                            (list (shepherd-service
                                                   (provision '(nixos-majordomo))
                                                   (requirement '(networking))
                                                   (start #~(make-forkexec-constructor
                                                             (list #$nixos-majordomo-program-file)))
                                                   (respawn? #f)
                                                   (auto-start? #t)
                                                   (one-shot? #t))))

                            (simple-service 'nixos-workstation shepherd-root-service-type
                                            (list (shepherd-service
                                                   (provision '(nixos-workstation))
                                                   (requirement '(networking))
                                                   (start #~(make-forkexec-constructor
                                                             (list #$nixos-workstation-program-file)))
                                                   (respawn? #f)
                                                   (auto-start? #t)
                                                   (one-shot? #t))))

                            (simple-service 'nixos-tor shepherd-root-service-type
                                            (list (shepherd-service
                                                   (provision '(nixos-tor))
                                                   (requirement '(networking))
                                                   (start #~(make-forkexec-constructor
                                                             (list #$nixos-tor-program-file)))
                                                   (respawn? #f)
                                                   (auto-start? #t)
                                                   (one-shot? #t))))

                            (simple-service 'nixos-antifilter shepherd-root-service-type
                                            (list (shepherd-service
                                                   (provision '(nixos-antifilter))
                                                   (requirement '(networking))
                                                   (start #~(make-forkexec-construcantifilter
                                                             (list #$nixos-antifilter-program-file)))
                                                   (respawn? #f)
                                                   (auto-start? #t)
                                                   (one-shot? #t))))

                            (simple-service 'nixos-zapret shepherd-root-service-type
                                            (list (shepherd-service
                                                   (provision '(nixos-zapret))
                                                   (requirement '(networking))
                                                   (start #~(make-forkexec-constructor
                                                             (list #$nixos-zapret-program-file)))
                                                   (respawn? #f)
                                                   (auto-start? #t)
                                                   (one-shot? #t))))

                            (simple-service 'ns-net-guix-workstation shepherd-root-service-type
                                            (list (shepherd-service
                                                   (provision '(ns-net-guix-workstation))
                                                   (requirement '(networking))
                                                   (start #~(make-forkexec-constructor
                                                             (list #$ns-net-guix-workstation-program-file)))
                                                   (respawn? #f)
                                                   (auto-start? #t)
                                                   (one-shot? #t))))

                            (simple-service 'fedora shepherd-root-service-type
                                            (list (shepherd-service
                                                   (provision '(fedora))
                                                   (requirement '(networking))
                                                   (start #~(make-forkexec-constructor
                                                             (list #$fedora-program-file)))
                                                   (respawn? #f)
                                                   (auto-start? #t)
                                                   (one-shot? #t))))

                            (simple-service 'guix-rde shepherd-root-service-type
                                            (list (shepherd-service
                                                   (provision '(guix-rde))
                                                   (requirement '(networking))
                                                   (start #~(make-forkexec-constructor
                                                             (list #$guix-rde-program-file)))
                                                   (respawn? #f)
                                                   (auto-start? #t)
                                                   (one-shot? #t))))

                            (simple-service 'guix-nanokvm shepherd-root-service-type
                                            (list (shepherd-service
                                                   (provision '(guix-nanokvm))
                                                   (requirement '(networking))
                                                   (start #~(make-forkexec-constructor
                                                             (list #$guix-nanokvm-program-file)))
                                                   (respawn? #f)
                                                   (auto-start? #t)
                                                   (one-shot? #t))))

                            (service virtlog-service-type
                                     (virtlog-configuration
                                      (max-clients 1000)))

                            (udev-rules-service 'kvm
                                                (udev-rule
                                                 "91-kvm-custom.rules"
                                                 "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0666\"\n"))

                            (udev-rules-service 'kvmfr
                                                (udev-rule
                                                 "99-kvmfr.rules"
                                                 "SUBSYSTEM==\"kvmfr\", OWNER=\"oleg\", GROUP=\"kvm\", MODE=\"0660\"\n"))

                            (service knot-resolver-service-type
                                     (knot-resolver-configuration
                                      (kresd-config-file
                                       (generate-kresd-file %private-ip-address))))

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
                                                    (value "192.168.0.192/24"))))
                                       (routes
                                        (list (network-route
                                               (destination "default")
                                               (gateway "192.168.0.1"))))
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
                                       (addresses '())))))
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
                                            (substitute-urls '("http://runc-kube1-guix-builder.guix.svc.cluster.local:5556"
                                                               "https://bordeaux.guix.gnu.org"
                                                               "https://substitutes.nonguix.org"
                                                               "http://ci.guix.trop.in"))))
                        (sysctl-service-type _ =>
                                             (sysctl-configuration
                                              (settings (append '(("kernel.sysrq" . "1")
                                                                  ("net.bridge.bridge-nf-call-iptables" . "0")
                                                                  ("vm.nr_hugepages" . "16384")
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
