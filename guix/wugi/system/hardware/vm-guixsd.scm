(define-module (wugi system hardware vm-guixsd)
  #:use-module (gnu)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages linux)
  #:use-module (gnu services networking)
  #:export (%vm-guixsd-hardware))

(define (%vm-guixsd-hardware)
  (operating-system
    (host-name "vm-guixsd")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))))

    (file-systems (cons* (file-system
                           (device (file-system-label "guix-root"))
                           (mount-point "/")
                           (type "ext4")
                           (flags '(shared))
                           (dependencies mapped-devices))
                         (file-system
                           (device (file-system-label "boot"))
                           (mount-point "/boot/efi")
                           (type "vfat"))
                         (file-system
                           (device "tmpfs")
                           (mount-point "/tmp")
                           (type "tmpfs")
                           (check? #f)
                           (flags '(no-dev))
                           (options "mode=1777,size=50%"))
                         %base-file-systems))

    (kernel linux-libre-with-bpf)

    (kernel-arguments '("net.ifnames=0"
                        "biosdevname=0"

                        "modprobe.blacklist=pcspkr,snd_pcsp"

                        ;; <https://wiki.archlinux.org/index.php/PCI_passthrough_via_OVMF#Setting_up_IOMMU>
                        ;; "iommu=pt"

                        ;; Enable LUKS TRIM/DISCARD pass-through.
                        "rd.luks.options=discard"))
    (users (append (list (user-account
                          (name "oleg")
                          (uid 1000)
                          (comment "Oleg Pykhalov")
                          (group "users")
                          (supplementary-groups '("wheel"))
                          (home-directory "/home/oleg")))
                   %base-user-accounts))
    (sudoers-file (plain-file "sudoers"
                              (string-join '("root ALL=(ALL) ALL"
                                             "%wheel ALL=(ALL) ALL"
                                             "oleg ALL=(ALL) NOPASSWD:ALL")
                                           "\n")))
    (packages %base-packages)
    (services %base-services)))
