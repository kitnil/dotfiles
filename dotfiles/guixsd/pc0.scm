;; -*- mode: scheme; -*-
;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu)
             (gnu services shepherd)
             (gnu services)
             (gnu system setuid)
             (guix gexp)
	     (nongnu packages linux)
	     (nongnu system linux-initrd)
             (bootloader grub)
             (config)
             (services kubernetes))
(use-service-modules avahi desktop dbus docker networking nix monitoring linux sound ssh virtualization xorg)
(use-package-modules audio linux screen ssh wm)

(use-modules (services backup)
             (utils package))

(define kvmfr-linux-module
  (@ (packages linux) kvmfr-linux-module))

(define container-guix-program
  (program-file "container-guix-program"
                #~(begin
                    (setenv "PATH"
                            "/run/setuid-programs:/root/.config/guix/current/bin:/run/current-system/profile/bin:/run/current-system/profile/sbin")
                    (execl #$(local-file "/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/13-guix-workstation-run.sh"
                                         #:recursive? #t)
                           "13-guix-workstation-run.sh"))))

(define container-guix-sway-autostart-program
  (program-file "container-guix-sway-autostart-program"
                #~(begin
                    (setenv "PATH"
                            "/run/setuid-programs:/root/.config/guix/current/bin:/run/current-system/profile/bin:/run/current-system/profile/sbin")
                    (execl #$(local-file "/home/oleg/.local/share/chezmoi/dotfiles/run/pc0/14-sway-run-all.sh"
                                         #:recursive? #t)
                           "sway-run-all"))))

(operating-system
  (host-name "pc0")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")


  (initrd microcode-initrd)
  (kernel linux)
  (firmware (cons* linux-firmware %base-firmware))

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader-removable)
               (targets '("/boot/efi"))))

  (kernel-loadable-modules (list kvmfr-linux-module
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

  (kernel-arguments '("net.ifnames=0"
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

                      "vfio-pci.ids=1002:7480,1002:ab30"

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
  (users (cons (user-account
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
                                        "libvirt")))
               %base-user-accounts))

  ;; Globally-installed packages.
  (packages (append (list screen sway)
                    (map package-from-program-file
                         (list restic-pc0-backup
                               restic-pc0-win10-backup))
                    %pc0-packages
                    %base-packages))

  (hosts-file
   (plain-file
    "hosts"
    "\
127.0.0.1	localhost	pc0
::1	localhost	pc0

192.168.0.144 kube1 kube1.home kube1.lan
192.168.0.192 kube3 kube3.home kube3.lan
"))

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
  (services (append (list (service avahi-service-type)
                          (service dhcp-client-service-type
                                   (dhcp-client-configuration
                                    (interfaces '("eth0"))))
                          (service openssh-service-type
                                   (openssh-configuration
                                    (openssh openssh-sans-x)
                                    ))
			  (udisks-service)
                          (service upower-service-type)
                          (service accountsservice-service-type)
                          (service colord-service-type)
                          (geoclue-service)
                          (service polkit-service-type)
                          (elogind-service)
                          (dbus-service)
                          (service ntp-service-type)

                          (service nix-service-type
                                   (nix-configuration
                                    (extra-config '("\
trusted-users = oleg root
binary-caches = https://cache.nixos.org/
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
"))))
                          (service kernel-module-loader-service-type
                                   '("amdgpu"
                                     "vfio-pci"

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
                                     "iptable_filter"))
                          (service containerd-service-type)
                          (service docker-service-type)
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

                         (service bluetooth-service-type
                                  (bluetooth-configuration
                                   (auto-enable? #t)
                                   (just-works-repairing 'confirm)
                                   (controller-mode 'dual)
                                   (min-connection-interval 7)
                                   (max-connection-interval 9)
                                   (connection-latency 0)
                                   (privacy 'device)))
                         udev-rules-service-xbox

                         (udev-rules-service 'kvm
                                             (udev-rule
                                              "91-kvm-custom.rules"
                                              "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0666\"\n"))

                         (udev-rules-service 'kvmfr
                                             (udev-rule
                                              "99-kvmfr.rules"
                                              "SUBSYSTEM==\"kvmfr\", OWNER=\"oleg\", GROUP=\"kvm\", MODE=\"0660\"\n"))

                         (service ladspa-service-type
                                  (ladspa-configuration (plugins (list swh-plugins))))
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
                                         (cons tty %default-console-font))
                                       '("tty1" "tty3" "tty4" "tty5" "tty6")))
                         (simple-service 'container-guix shepherd-root-service-type
                                         (list
                                          (shepherd-service
                                           (provision '(container-guix))
                                           (auto-start? #f)
                                           (one-shot? #t)
                                           (documentation "Provision Guix container.")
                                           (requirement '())
                                           (start #~(make-forkexec-constructor
                                                     (list #$container-guix-program)))
                                           (respawn? #f))))
                         (simple-service 'container-guix-sway-autostart shepherd-root-service-type
                                         (list
                                          (shepherd-service
                                           (provision '(container-guix-sway-autostart))
                                           (auto-start? #f)
                                           (documentation "Run programs in Sway inside Guix container.")
                                           (requirement '())
                                           (start #~(make-forkexec-constructor
                                                     (list #$container-guix-sway-autostart-program)))
                                           (respawn? #f)
                                           (stop #~(make-kill-destructor))))))
                    (modify-services
                        (filter (lambda (service)
                                (let ((value (service-value service)))
                                  (not (and (mingetty-configuration? value)
                                            (string= (mingetty-configuration-tty value)
                                                     "tty2")))))
                              (modify-services %base-services
                                (guix-service-type config =>
                                                   (guix-configuration
                                                    (authorized-keys (append (list (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/guix.wugi.info.pub")
                                                                                   (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm1.wugi.info.pub")
                                                                                   (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm2.wugi.info.pub")
                                                                                   (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/mirror.brielmaier.net.pub")
                                                                                   (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/substitutes.nonguix.org.pub")
                                                                                   (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/bordeaux.guix.gnu.org.pub"))
                                                                             %default-authorized-guix-keys))
                                                    (substitute-urls '("https://guix.wugi.info"
                                                                       "https://bordeaux.guix.gnu.org"
                                                                       "https://substitutes.nonguix.org"))))
                                ;; (sysctl-service-type _ =>
                                ;;                      (sysctl-configuration
                                ;;                       (settings (append '(("net.ipv4.ip_forward" . "1")
                                ;;                                           ("net.ipv4.conf.all.rp_filter" . "0")
                                ;;                                           ("net.ipv4.conf.default.rp_filter" . "0"))
                                ;;                                         %default-sysctl-settings))))
                                ))
                      (delete console-font-service-type)))))
