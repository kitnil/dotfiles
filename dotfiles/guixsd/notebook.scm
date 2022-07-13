;; This is an operating system configuration template
;; for a "desktop" setup without full-blown desktop
;; environments.

(use-modules (gnu)
	     (gnu system nss)
	     (nongnu packages linux)
	     (nongnu system linux-initrd))

(use-service-modules desktop networking ssh)

(use-package-modules bootloaders certs vpn wm terminals xfce)

(use-service-modules desktop dbus networking xorg)

(operating-system
  (host-name "notebook")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (initrd microcode-initrd)
  (kernel linux-5.15)
  (firmware (cons* linux-firmware %base-firmware))

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))))

    (mapped-devices (list (mapped-device
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
                         (device (uuid "0A05-C141" 'fat))
                         (mount-point "/boot/efi")
                         (type "vfat")))
                 %base-file-systems))

  (users (cons (user-account
                (name "oleg")
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video")))
               %base-user-accounts))

  ;; Add a bunch of window managers; we can choose one at
  ;; the log-in screen with F1.
  (packages (append (list
                     ;; window managers
                     ;;sway dmenu
                     ;; terminal emulator
                     ;; for HTTPS access
		     openvpn
sway
alacritty
;ratpoison
;xfce4-terminal
                     nss-certs)
                    %base-packages))

  ;; Use the "desktop" services, which include the X11
  ;; log-in service, networking with NetworkManager, and more.
  (services (append (list
                     (service wpa-supplicant-service-type)    ;needed by NetworkManager
                     (service network-manager-service-type)
                     (service openssh-service-type))

                    (list ;; (screen-locker-service slock)
                          (udisks-service)
                          (service upower-service-type)
                          (service accountsservice-service-type)
                          (service colord-service-type)
                          (geoclue-service)
                          (service polkit-service-type)
                          (elogind-service)
                          (dbus-service)
                          (service ntp-service-type))

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
                                          (substitute-urls '("https://ci.guix.gnu.org"
                                                             "https://guix.wugi.info"
                                                             "https://mirror.brielmaier.net"))))
                      ;; (sysctl-service-type _ =>
                      ;;                      (sysctl-configuration
                      ;;                       (settings (append '(("net.ipv4.ip_forward" . "1")
                      ;;                                           ("net.ipv4.conf.all.rp_filter" . "0")
                      ;;                                           ("net.ipv4.conf.default.rp_filter" . "0"))
                      ;;                                         %default-sysctl-settings))))
                      )))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
