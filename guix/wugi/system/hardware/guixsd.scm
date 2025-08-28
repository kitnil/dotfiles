(define-module (wugi system hardware guixsd)
  #:use-module (gnu)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages ratpoison)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services xorg)
  #:use-module (nongnu system linux-initrd)
  #:use-module (wugi bootloader grub)
  #:use-module (nongnu packages linux)
  #:export (%guixsd-hardware))

(define (%guixsd-hardware)
  (operating-system
    (host-name "guixsd")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader-removable)
                 (targets '("/boot1/efi"
                            "/boot2/efi"
                            "/boot3/efi"))))

    (initrd-modules (append '("raid456" "vfio-pci") %base-initrd-modules))

    (mapped-devices (list (mapped-device
                           (source
                            (list "/dev/sda2" "/dev/sdc2" "/dev/sdd2"))
                           (target "/dev/md0")
                           (type raid-device-mapping))
                          (mapped-device
                           (source (uuid "0c0175eb-64ae-46f7-9a54-43d4b65b0818"))
                           (target "guix-root")
                           (type luks-device-mapping))))

    (file-systems (cons* (file-system
                           (device (file-system-label "guix-root"))
                           (mount-point "/")
                           (type "ext4")
                           (flags '(shared))
                           (dependencies mapped-devices))
                         (file-system
                           (device (file-system-label "boot1"))
                           (mount-point "/boot1/efi")
                           (type "vfat"))
                         (file-system
                           (device (file-system-label "boot2"))
                           (mount-point "/boot2/efi")
                           (type "vfat"))
                         (file-system
                           (device (file-system-label "boot3"))
                           (mount-point "/boot3/efi")
                           (type "vfat"))
                         (file-system
                           (device "tmpfs")
                           (mount-point "/tmp")
                           (type "tmpfs")
                           (check? #f)
                           (flags '(no-dev))
                           (options "mode=1777,size=50%"))
                         (file-system
                           (device "kubevirt")
                           (mount-point "/var/run/kubevirt")
                           (type "tmpfs")
                           (check? #f)
                           (flags '(shared))
                           (create-mount-point? #t))
                         %base-file-systems))

    (initrd microcode-initrd)
    (kernel linux-6.6)
    (firmware (append (list linux-firmware)
                      %base-firmware))

    (kernel-arguments '("net.ifnames=0"
                        "biosdevname=0"

                        "mitigations=off"

                        "modprobe.blacklist=pcspkr,snd_pcsp"

                        "kvm.ignore_msrs=1"
                        "kvm.report_ignored_msrs=0"

                        "vfio-pci.ids=1002:7480,1002:ab30"

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
