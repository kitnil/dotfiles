;; To build the QEMU image run the following commands:
;;
;; guix build -f build.scm
;; /gnu/store/...-packer/bin/packer-build-guix

(use-modules (gnu packages guile)
             (guix gexp)
             (guix modules)
             (guix utils))

;; TODO: Use Packer from Guix package collection
(define %packer
  (and=> (getenv "HOME")
         (lambda (home)
           (string-append home "/.nix-profile/bin/packer"))))

(define %packer-operating-system
  `(("provisioners" . #((("type" . "shell")
                         ("inline" . #("set -x"
                                       "parted -s --align=none /dev/vda mklabel msdos mkpart primary ext4 1MiB 100%"
                                       "mkfs.ext4 -L my-root /dev/vda1"
                                       "mount LABEL=my-root /mnt"
                                       "herd start cow-store /mnt"
                                       "mkdir /mnt/etc")))
                        (("type" . "file")
                         ("source" . "bare-bones.tmpl")
                         ("destination" . "/mnt/etc/config.scm"))
                        (("type" . "shell")
                         ("max_retries" . 3)
                         ("inline" . #("guix system init /mnt/etc/config.scm /mnt")))
                        (("type" . "shell")
                         ("inline" . #("reboot"))
                         ("expect_disconnect" . #t))))
    ("min_packer_version" . "1.4.0")
    ("builders" . #((("vm_name" . "guix")
                     ("type" . "qemu")
                     ("ssh_username" . "root")
                     ("ssh_timeout" . "10m")
                     ("ssh_password" . "password")
                     ("shutdown_command" . "shutdown")
                     ("memory" . 2048)
                     ;; TODO: Find non-compressed Guix ISO or fix Packer cache ignore in case ISO is compressed
                     ("iso_url" . "https://ftp.gnu.org/gnu/guix/guix-system-install-1.3.0.x86_64-linux.iso")
                     ("iso_checksum" . "sha256:f2b30458fa1736eeee3b82f34aab1d72f3964bef0477329bb75281d2b7bb6d4b")
                     ("headless" . #f)
                     ("boot_keygroup_interval" . "2s")
                     ("boot_wait" . "40s")
                     ("boot_command" . #("<enter><wait1s>"
                                         "<end><wait1s><up><wait1s><up><wait1s><enter><wait1s>"
                                         "<down><wait1s>"
                                         "<enter><wait1s>"
                                         "<enter><wait1s>"
                                         "passwd root<enter>password<enter>password<enter>"
                                         "guix package -i openssh<enter><wait1s>"
                                         "herd start ssh-daemon<enter>"))
                     ("accelerator" . "kvm"))))))

(define guix.json
  (mixed-text-file "guix.json"
                   (with-extensions (list guile-json-4)
                     (with-imported-modules (source-module-closure '((json builder)))
                       #~(begin
                           (use-modules (json builder))
                           (scm->json-string '#$%packer-operating-system))))))

(file-union "packer"
            `(("configuration.scm" ,(local-file "bare-bones.tmpl"))
              ("bin/packer-build-guix" ,(program-file "packer-build"
                                                      (with-imported-modules '((guix build utils))
                                                        #~(begin
                                                            (use-modules (guix build utils))
                                                            (define build-directory
                                                              (if (getenv "PACKER_CACHE_DIR")
                                                                  (getenv "PACKER_CACHE_DIR")
                                                                  "packer-build"))
                                                            (mkdir-p build-directory)
                                                            (chdir build-directory)
                                                            (if (file-exists? "bare-bones.tmpl")
                                                                (delete-file "bare-bones.tmpl"))
                                                            (copy-file #$(local-file "bare-bones.tmpl") "bare-bones.tmpl")
                                                            (chmod "bare-bones.tmpl" #o644)
                                                            (system* #$%packer "build" #$guix.json)))))))
