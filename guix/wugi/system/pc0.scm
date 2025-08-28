;; -*- mode: scheme; -*-
;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(define-module (wugi system pc0)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages wm)
  #:use-module ((gnu services) #:select (delete
                                         service-type
                                         service-extension
                                         service-value
                                         service
                                         simple-service
                                         modify-services
                                         activation-service-type))
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dns)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)
  #:use-module (gnu services monitoring)
  #:use-module (gnu services networking)
  #:use-module (gnu services nix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sound)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (wugi services containers)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system setuid)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (wugi bootloader grub)
  #:use-module (wugi config)
  #:use-module ((wugi packages linux) #:select (kvmfr-linux-module))
  #:use-module (wugi services backup)
  #:use-module (wugi services kubernetes)
  #:use-module (wugi utils)
  #:use-module (wugi utils package)
  #:use-module (srfi srfi-26)
  #:export (%pc0))

(define %private-ip-address
  "192.168.0.192")

(define container-guix-program
  (program-file "container-guix-program"
                #~(begin
                    (setenv "PATH"
                            "/run/setuid-programs:/root/.config/guix/current/bin:/run/current-system/profile/bin:/run/current-system/profile/sbin")
                    (execl #$(local-file (string-append %distro-directory "/dotfiles/run/pc0/13-guix-workstation-run.sh")
                                         #:recursive? #t)
                           "13-guix-workstation-run.sh"))))

(define container-guix-sway-autostart-program
  (program-file "container-guix-sway-autostart-program"
                #~(begin
                    (setenv "PATH"
                            "/run/setuid-programs:/root/.config/guix/current/bin:/run/current-system/profile/bin:/run/current-system/profile/sbin")
                    (execl #$(local-file (string-append %distro-directory "/dotfiles/run/pc0/14-sway-run-all.sh")
                                         #:recursive? #t)
                           "sway-run-all"))))

(define (%pc0)
  (operating-system
    (host-name "pc0")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")


    (initrd microcode-initrd)
    (initrd-modules (append '("vfio-pci") %base-initrd-modules))
    (kernel linux)
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
                           (type "ext4")
                           (flags '(shared)))
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
                           (device "tmpfs")
                           (mount-point "/mnt/guix-workstation/tmp")
                           (type "tmpfs")
                           (check? #f)
                           (flags '(no-dev))
                           (options "mode=1777,size=10%")))
                   %base-file-systems))

    (kernel-arguments '(
                        "net.ifnames=0"
                        "biosdevname=0"

                        "modprobe.blacklist=pcspkr,snd_pcsp"

                        ;; Enable LUKS TRIM/DISCARD pass-through.
                        "rd.luks.options=discard"

                        ;; <https://wiki.archlinux.org/index.php/PCI_passthrough_via_OVMF#Setting_up_IOMMU>
                        ;; "iommu=pt"

                        ;; <https://pve.proxmox.com/wiki/PCI_Passthrough>
                        ;; Some Windows applications like GeForce Experience,
                        ;; Passmark Performance Test and SiSoftware Sandra can
                        ;; crash the VM.
                        "kvm.ignore_msrs=1"

                        "vfio-pci.ids=1002:7550,1002:ab40"

                        ;; (#934) · Issues · drm / amd · GitLab
                        ;; <https://gitlab.freedesktop.org/drm/amd/-/issues/934>
                        ;; "amdgpu.audio=0"
                        ;; "amdgpu.gpu_recovery=1"
                        ;; "amdgpu.noretry=0"
                        ;; "amdgpu.ppfeaturemask=0xfffffffb"

                        ;; https://gitlab.freedesktop.org/drm/amd/-/issues/2220
                        ;; [amdgpu]] *ERROR* ring sdma0 timeout
                        ;;
                        ;;
                        ;; https://wiki.archlinux.org/title/AMDGPU
                        ;;
                        ;; Freezes with "[drm] IP block:gmc_v8_0 is hung!" kernel error
                        ;;
                        ;; If you experience freezes and kernel crashes during a
                        ;; GPU intensive task with the kernel error " [drm] IP
                        ;; block:gmc_v8_0 is hung!" [6], a workaround is to set
                        ;; amdgpu.vm_update_mode=3 as kernel parameters to force
                        ;; the GPUVM page tables update to be done using the
                        ;; CPU. Downsides are listed here [7].
                        ;;
                        ;;
                        ;; [7]: https://gitlab.freedesktop.org/drm/amd/-/issues/226#note_308665
                        ;;
                        ;; I think it just means systems with large VRAM so it
                        ;; will require large BAR for mapping. But I am not sure
                        ;; on that point.
                        ;;
                        ;; vm_update_mode=3 means GPUVM page tables update is
                        ;; done using CPU. By default we do it using DMA engine
                        ;; on the ASIC. The log showed a hang in this engine so
                        ;; I assumed there is something wrong with SDMA commands
                        ;; we submit.
                        ;;
                        ;; I assume more CPU utilization as a side effect and
                        ;; maybe slower rendering.
                        ;; "amdgpu.vm_update_mode=3"
                        ))

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
                                                  "docker"
                                                  "kvm"
                                                  "input"
                                                  "libvirt"))))
                   %base-user-accounts))

    ;; Globally-installed packages.
    (packages (append (list screen sway)
                      (map package-from-program-file
                           (list restic-pc0-backup
                                 restic-pc0-win10-backup))
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
    (services (append (list (service openssh-service-type
                                     (openssh-configuration
                                      (openssh openssh-sans-x)
                                      (permit-root-login 'prohibit-password)))
                            (udisks-service)
                            (service upower-service-type)
                            (service accountsservice-service-type)
                            (service colord-service-type)
                            (geoclue-service)
                            (service polkit-service-type)
                            (elogind-service)
                            (dbus-service)

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
                                       "ddcci_backlight"))
                            (service containerd-service-type)
                            (service docker-service-type)

                            (service runc-container-service-type
                                     (runc-container-configuration
                                      (bundle "/srv/runc/guix-workstation")
                                      (name "guix-workstation")))

                            (service runc-container-service-type
                                     (runc-container-configuration
                                      (bundle "/srv/runc/guix-rde")
                                      (name "guix-rde")))

                            (service runc-container-service-type
                                     (runc-container-configuration
                                      (bundle "/srv/runc/nixos-workstation")
                                      (name "nixos-workstation")))

                            (service kubelet-service-type
                                     (kubelet-configuration
                                      (kubelet "/nix/store/lp8ch8l5dn4bcp056cpr1gfyb9i8zi54-kubernetes-1.25.4/bin/kubelet")
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

                            (udev-rules-service 'kvm
                                                (udev-rule
                                                 "91-kvm-custom.rules"
                                                 "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0666\"\n"))

                            (udev-rules-service 'kvmfr
                                                (udev-rule
                                                 "99-kvmfr.rules"
                                                 "SUBSYSTEM==\"kvmfr\", OWNER=\"oleg\", GROUP=\"kvm\", MODE=\"0660\"\n"))

                            (service libvirt-service-type
                                     (libvirt-configuration
                                      ;; XXX: Specify listen-addr after adding networking requirement.
                                      ;;
                                      ;; (listen-addr "192.168.0.192")
                                      (listen-tcp? #t)
                                      (auth-tcp "none")))
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

                            (service virtlog-service-type
                                     (virtlog-configuration
                                      (max-clients 1000)))
                            (service console-font-service-type
                                     (map (lambda (tty)
                                            (append (list tty)
                                                    %default-console-font))
                                          '("tty1"
                                            "tty2"
                                            "tty3"
                                            "tty4"
                                            "tty5"
                                            "tty6")))

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
                                       (provision '(br0-link))
                                       (links (list
                                               (network-link
                                                (name "br0")
                                                (type 'bridge)
                                                (arguments '()))))
                                       (addresses '()))
                                      (static-networking
                                       (provision '(br0))
                                       (requirement '(br0-link))
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
                                                                  ("net.bridge.bridge-nf-call-iptables" . "0"))
                                                                %default-sysctl-settings))))
                        (delete console-font-service-type))))))
