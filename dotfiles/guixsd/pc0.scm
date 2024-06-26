;; -*- mode: scheme; -*-
;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu)
	     (nongnu packages linux)
	     (nongnu system linux-initrd)
             (bootloader grub)
             (config)
             (services kubernetes))
(use-service-modules desktop dbus docker networking nix monitoring sound ssh xorg)
(use-package-modules screen ssh wm)

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
                         (type "vfat")))
                 %base-file-systems))

  (kernel-arguments '("net.ifnames=0"
                      "biosdevname=0"

                      "modprobe.blacklist=pcspkr,snd_pcsp"

                      ;; Enable LUKS TRIM/DISCARD pass-through.
                      "rd.luks.options=discard"))

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
                                        "audio" "video")))
               %base-user-accounts))

  ;; Globally-installed packages.
  (packages (append (list screen sway)
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
  (services (append (list (service dhcp-client-service-type)
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
                                    (kubevirt? #t))))
                    (modify-services
                     (modify-services %base-services
                                      (guix-service-type config =>
                                                         (guix-configuration
                                                          (authorized-keys (append (list (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/guix.wugi.info.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm1.wugi.info.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm2.wugi.info.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm3.wugi.info.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/jenkins.intr.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/spb.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/mirror.brielmaier.net.pub")
                                                                                         (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/substitutes.nonguix.org.pub"))
                                                                                   %default-authorized-guix-keys))
                                                          (substitute-urls '("http://ci.guix.gnu.org.wugi.info"
                                                                             "https://guix.wugi.info"
                                                                             "https://substitutes.nonguix.org"))))
                                      ;; (sysctl-service-type _ =>
                                      ;;                      (sysctl-configuration
                                      ;;                       (settings (append '(("net.ipv4.ip_forward" . "1")
                                      ;;                                           ("net.ipv4.conf.all.rp_filter" . "0")
                                      ;;                                           ("net.ipv4.conf.default.rp_filter" . "0"))
                                      ;;                                         %default-sysctl-settings))))
                                      )
))))
