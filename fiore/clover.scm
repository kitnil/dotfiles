;; GuixSD configuration file for the desktop machine.
;; Copyright © 2017, 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (gnu) (gnu system nss))

(use-service-modules ssh networking)

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
   (substitute-urls '("https://guix.duckdns.org" "https://ci.guix.info"))
   (max-silent-time 7200)
   (timeout (* 4 max-silent-time))
   (extra-options '("--max-jobs=4" "--cores=2"
                    "--cache-failures"
                    "--gc-keep-outputs=yes"
                    "--gc-keep-derivations=yes"))))

(define %guix-system-thinkpad-x200
  (operating-system
    (host-name "clover")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

    (bootloader (bootloader-configuration (bootloader grub-bootloader) (target "/dev/sda")))

                                        ;  (kernel linux-nonfree)
                                        ;  (firmware (list firmware-non-free))

    (file-systems (cons (file-system
                          (device "clover-root")
                          (title 'label)
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))

    (users (cons (user-account
                  (name "oleg")
                  (uid 1000)
                  (comment "Oleg Pykhalov")
                  (group "users")
                  (supplementary-groups '("wheel" "audio" "video"))
                  (home-directory "/home/oleg"))
                 %base-user-accounts))

    (packages %base-packages)

    (services %base-services)

    ;; Allow resolution of '.local' host names with mDNS.
    ;; (name-service-switch %mdns-host-lookup-nss)
    ))

%guix-system-thinkpad-x200

#;(list (machine
       (operating-system %guix-system-thinkpad-x200)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "192.168.100.20")
                       (system "x86_64-linux")
                       (identity "/home/oleg/.ssh/id_rsa_guix")
                       ;; (build-locally? #f)
                       ))))
