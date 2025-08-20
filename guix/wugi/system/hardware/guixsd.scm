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

    (initrd-modules (cons "raid456" %base-initrd-modules))

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
    (kernel linux-6.12)
    (firmware (append (list linux-firmware)
                      %base-firmware))

    (kernel-arguments '("net.ifnames=0"
                        "biosdevname=0"

                        "modprobe.blacklist=pcspkr,snd_pcsp"

                        ;; <https://wiki.archlinux.org/index.php/PCI_passthrough_via_OVMF#Setting_up_IOMMU>
                        ;; "iommu=pt"

                        "kvm.ignore_msrs=1"
                        "report_ignored_msrs=0"
                        ;; "vfio-pci.ids=1002:7480,1002:ab30"
                        ;; "disable_vga=1"

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
