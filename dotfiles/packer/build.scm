;; To build the QEMU image run the following commands:
;;
;; BUILD_LOCAL_GIT_REPOSITORY=true guix build -f build.scm
;; /gnu/store/...-packer/bin/packer-build-guix

(use-modules (gnu packages guile)
             (guix gexp)
             (guix modules)
             (guix utils)
             (ice-9 match)
             (ice-9 ftw)
             (srfi srfi-1)
             (srfi srfi-26))

;; TODO: Use Packer from Guix package collection
(define %packer
  (and=> (getenv "HOME")
         (lambda (home)
           (string-append home "/.nix-profile/bin/packer"))))

(define %build-local-git-repository
  (and=> (getenv "BUILD_LOCAL_GIT_REPOSITORY")
         (lambda (environment)
           (or (string= environment "true")
               (string= environment "t")))))

(define %packer-operating-system
  `(("provisioners" . #((("type" . "shell")
                         ("inline" . #("set -x"
                                       "parted -s --align=none /dev/vda mklabel msdos mkpart primary ext4 1MiB 100%"
                                       "pvcreate /dev/vda1"
                                       "vgcreate vg0 /dev/vda1"
                                       "lvcreate -L 8G vg0 -n guix"
                                       "mkfs.ext4 -L guix /dev/vg0/guix"
                                       "mount LABEL=guix /mnt"
                                       "herd start cow-store /mnt"
                                       "mkdir /mnt/etc")))
                        (("type" . "file")
                         ("source" . "bare-bones.tmpl")
                         ("destination" . "/mnt/etc/config.scm"))
                        (("type" . "file")
                         ("source" . "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/guix.wugi.info.pub")
                         ("destination" . "/root/guix.wugi.info.pub"))
                        (("type" . "shell")
                         ("inline" . #("set -x"
                                       "guix archive --authorize < /root/guix.wugi.info.pub")))
                        (("type" . "shell")
                         ("max_retries" . 3)
                         ("inline" . #("set -x"
                                       "guix system init --substitute-urls=\"https://guix.wugi.info https://ci.guix.gnu.org\" /mnt/etc/config.scm /mnt")))
                        (("type" . "shell")
                         ("inline" . #("set -x"
                                       "reboot"))
                         ("expect_disconnect" . #t))
                        (("type" . "shell")
                         ("max_retries" . 3)
                         ("inline" . #("set -x"
                                       "guix pull"
                                       "hash guix"
                                       "guix system reconfigure /etc/config.scm"
                                       ,@(if %build-local-git-repository
                                             '("guix time-machine --disable-authentication --url=https://cgit.duckdns.org/git/guix/guix -- system build /etc/config.scm")
                                             '()))))
                        (("type" . "shell")
                         ("inline" . #("set -x"
                                       "reboot"))
                         ("expect_disconnect" . #t))))
    ("min_packer_version" . "1.4.0")
    ("builders" . #((("vm_name" . "guix")
                     ("type" . "qemu")
                     ("ssh_username" . "root")
                     ("ssh_timeout" . "10m")
                     ("ssh_password" . "password")
                     ("shutdown_command" . "shutdown")
                     ("disk_size" . 32768)
                     ("memory" . 4096)
                     ("cpus" . 4)
                     ;; TODO: Find non-compressed Guix ISO or fix Packer cache ignore in case ISO is compressed
                     ("iso_url" . "https://ftp.gnu.org/gnu/guix/guix-system-install-1.3.0.x86_64-linux.iso")
                     ("iso_checksum" . "sha256:f2b30458fa1736eeee3b82f34aab1d72f3964bef0477329bb75281d2b7bb6d4b")
                     ("headless" . #t)
                     ("boot_keygroup_interval" . "2s")
                     ("boot_wait" . "40s")
                     ("boot_command" . #("<enter><wait1s>"
                                         "<end><wait1s><up><wait1s><up><wait1s><enter><wait1s>"
                                         "<down><wait1s>"
                                         "<enter><wait1s>"
                                         "<enter><wait1s>"
                                         "passwd root<enter>password<enter>password<enter>"
                                         "guix package -i openssh lvm2<enter><wait1s>"
                                         "herd start ssh-daemon<enter>"))
                     ("accelerator" . "kvm")
                     ("output_directory" . ,(string-append "/mnt/packer/builds/guix."
                                                           (number->string
                                                            (1+ (first
                                                                 (sort (map string->number
                                                                            (map (cut string-drop <> (string-length "guix."))
                                                                                 (map first
                                                                                      (filter (match-lambda
                                                                                                ((name a b) (string-prefix? "guix." name))
                                                                                                (_ #f))
                                                                                              (file-system-tree "/mnt/packer/builds")))))
                                                                       >)))))))))))

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
                                                            (if (file-exists? "bare-bones.tmpl")
                                                                (delete-file "bare-bones.tmpl"))
                                                            (copy-file #$(local-file "bare-bones.tmpl") "bare-bones.tmpl")
                                                            (chmod "bare-bones.tmpl" #o644)
                                                            (system* #$%packer "build" #$guix.json)))))))
