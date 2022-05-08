(define-module (bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (guix gexp)
  #:export (grub-efi-bootloader-removable))

(define install-grub-efi-removable
  #~(lambda (bootloader efi-dir mount-point)
      ;; There is nothing useful to do when called in the context of a disk
      ;; image generation.
      (when efi-dir
        ;; Install GRUB onto the EFI partition mounted at EFI-DIR, for the
        ;; system whose root is mounted at MOUNT-POINT.
        (let ((grub-install (string-append bootloader "/sbin/grub-install"))
              (install-dir (string-append mount-point "/boot"))
              ;; When installing Guix, it's common to mount EFI-DIR below
              ;; MOUNT-POINT rather than /boot/efi on the live image.
              (target-esp (if (file-exists? (string-append mount-point efi-dir))
                              (string-append mount-point efi-dir)
                              efi-dir)))
          ;; Tell 'grub-install' that there might be a LUKS-encrypted /boot or
          ;; root partition.
          (setenv "GRUB_ENABLE_CRYPTODISK" "y")
          (invoke/quiet grub-install
                        "--boot-directory" install-dir
                        "--efi-directory" target-esp

                        ;; do not write to nvram on the motherboard
                        "--no-nvram"
                        "--target=x86_64-efi"
                        "--removable")))))

(define grub-efi-bootloader-removable
  (bootloader
   (inherit grub-efi-bootloader)
   (name 'grub-efi-bootloader-removable)
   (installer install-grub-efi-removable)))
