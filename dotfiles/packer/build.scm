;; guix build -f build.scm

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
                     ("iso_url" . "/home/oleg/Downloads/guix-system-install-1.2.0.x86_64-linux.iso")
                     ("iso_checksum" . "sha256:423fa3a6b877a597e90d95c388714e4996dbfae8339f718bb57cce3954955dc7")
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
                                                      #~(begin
                                                          (mkdir "packer-build")
                                                          (chdir "packer-build")
                                                          (copy-file #$(local-file "bare-bones.tmpl") "bare-bones.tmpl")
                                                          (system* #$%packer "build" #$guix.json))))))
