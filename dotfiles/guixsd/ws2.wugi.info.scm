;; This is an operating system configuration template
;; for a "desktop" setup without full-blown desktop
;; environments.

(use-modules (gnu)
             (gnu system nss)
             (services autossh))
(use-service-modules desktop xorg ssh)
(use-package-modules bootloaders certs emacs emacs-xyz ratpoison suckless wm
                     xorg)

(operating-system
  (host-name "gnu")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")))

  ;; Assume the target root file system is labelled "my-root",
  ;; and the EFI System Partition has UUID 1234-ABCD.
  (file-systems (append
                 (list (file-system
                         (device (file-system-label "nixos"))
                         (mount-point "/")
                         (type "ext4"))
                       (file-system
                         (device (uuid "6917-FF8C" 'fat))
                         (mount-point "/boot/efi")
                         (type "vfat")))
                 %base-file-systems))

  (users (cons (user-account
                (name "oleg")
                (comment "Oleg Pykhalov")
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video")))
               %base-user-accounts))

  ;; Add a bunch of window managers; we can choose one at
  ;; the log-in screen with F1.
  (packages (append (list
                     ;; window managers
                     ratpoison ; nano
                     ;; emacs
                     ;; terminal emulator
                     xterm

                     ;; for HTTPS access
                     nss-certs)
                    %base-packages))

  ;; Use the "desktop" services, which include the X11
  ;; log-in service, networking with NetworkManager, and more.
  (services (append (list (service openssh-service-type)
                          (service slim-service-type)
                          (service (@ (services autossh) autossh-service-type)
                                   ((@ (services autossh) autossh-configuration)
                                    (autossh-client-config
                                     (autossh-client-configuration
                                      (hosts (list (autossh-client-host-configuration
                                                    (host "back.wugi.info")
                                                    (identity-file "/etc/autossh/id_rsa")
                                                    (strict-host-key-checking? #f)
                                                    (user "tipanova-34-1-ssh-tunnel")
                                                    (user-known-hosts-file "/dev/null")
                                                    (extra-options
                                                     "
RemoteForward 0.0.0.0:10022 127.0.0.1:22
Compression yes
ExitOnForwardFailure yes
ServerAliveInterval 30
ServerAliveCountMax 3"))))))
                                    (host "back.wugi.info"))))
                    (modify-services (modify-services %desktop-services
                                       (delete gdm-service-type))
                      (guix-service-type config =>
                                         (guix-configuration
                                          (inherit config)
                                          (authorized-keys
                                           (append (list (local-file "./key.pub"))
                                                   %default-authorized-guix-keys)))))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss)

  (sudoers-file (plain-file "sudoers"
                            (string-join `("root ALL=(ALL) ALL"
                                           "%wheel ALL=(ALL) ALL"
                                           "oleg ALL=(ALL) NOPASSWD:ALL")
                                         "\n"))))
