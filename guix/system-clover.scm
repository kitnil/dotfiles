(use-modules (gnu)
             (gnu system nss)
             (linux-nonfree)
             (guix store))

(use-service-modules ssh
                     desktop
                     xorg
                     cups
                     pm
                     version-control
                     mail)

(use-package-modules bootloaders
                     admin
                     emacs
                     cups
                     wm
                     certs
                     fonts
                     xdisorg
                     cryptsetup
                     bash)

(define 20-intel.conf "
# Fix tearing on intel
# https://wiki.archlinux.org/index.php/Intel_Graphics
# https://github.com/8p8c/my-guix/blob/master/config.scm
Section \"Device\"
   Identifier  \"Intel Graphics\"
   Driver      \"intel\"
   Option      \"TearFree\" \"true\"
EndSection
")

(define %guix-daemon-config
  (guix-configuration
   (substitute-urls
    (append %default-substitute-urls '("http://magnolia.local")))
   (max-silent-time 7200)
   (timeout (* 4 max-silent-time))
   (extra-options '("--max-jobs=4" "--cores=2"
                    "--cache-failures"
                    "--gc-keep-outputs" "--gc-keep-derivations"))))

(define %custom-desktop-services
  (modify-services %desktop-services
    (special-files-service-type config => `(("/bin/sh"
                                             ,(file-append
                                               bash "/bin/sh"))
                                            ("/usr/bin/env"
                                             ,(file-append
                                               coreutils "/bin/env"))))
    (guix-service-type config => %guix-daemon-config)
    (slim-service-type config => (slim-configuration
                                  (inherit config)
                                  (startx
                                   (xorg-start-command
                                    #:configuration-file
                                    (xorg-configuration-file
                                     #:extra-config (list 20-intel.conf))))
                                  (auto-login? #t)
                                  (default-user "natsu")))))

(operating-system
  (host-name "clover")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (grub-configuration (device "/dev/sda")))

  (kernel linux-nonfree)
  (firmware (list firmware-non-free))

  (file-systems (cons (file-system
                        (device "clover-root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "natsu")
                (uid 1000)
                (comment "Oleg Pykhalov")
                (group "users")
                (supplementary-groups '("wheel"
                                        "audio" "video"))
                (home-directory "/home/natsu"))
               %base-user-accounts))

  (packages (cons* i3-wm
                   i3status
                   cups
                   rofi
                   cryptsetup
                   emacs
                   emacs-guix
                   nss-certs
                   font-dejavu
                   font-liberation
                   wpa-supplicant
                   %base-packages))

  (services (cons* (service tlp-service-type
                            (tlp-configuration
                             (wol-disable? #f)))
                   (service openssh-service-type
                            (openssh-configuration
                             (port-number 22)))
                   (service cups-service-type
                            (cups-configuration
                             (web-interface? #t)
                             (extensions
                              (list cups-filters hplip))))
                   (service guix-publish-service-type
                            (guix-publish-configuration
                             (host "0.0.0.0")))
                   (service git-daemon-service-type
                            (git-daemon-configuration
                             (user-path "")))
                   (dovecot-service
                    #:config (dovecot-configuration
                              (mail-location
                               (string-append
                                "maildir:~/Maildir:INBOX=~/Maildir/INBOX:"
                                "LAYOUT=fs"))
                              (disable-plaintext-auth? #f)
                              (listen '("127.0.0.1"))))
                   %custom-desktop-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
