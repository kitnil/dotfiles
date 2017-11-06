;; GuixSD configuration file for the game machine.
;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (gnu)
             (wigust packages games))
(use-service-modules desktop xorg)
(use-package-modules certs fonts bash xfce)

(define %custom-desktop-services
  (modify-services %desktop-services
    (special-files-service-type config => `(("/bin/sh"
                                             ,(file-append
                                               bash "/bin/sh"))
                                            ("/usr/bin/env"
                                             ,(file-append
                                               coreutils "/bin/env"))))
    (slim-service-type config => (slim-configuration
                                  (inherit config)
                                  (auto-login? #t)
                                  (auto-login-session (file-append xfce
                                                                   "/bin/startxfce4"))
                                  (default-user "alice")))))

(operating-system
  (host-name "xfce")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (grub-configuration (target "/dev/sda")
                                  (terminal-outputs '(console))))
  
  (file-systems (cons (file-system
                        (device "my-root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons* (user-account
                 (name "alice")
                 (comment "Bob's sister")
                 (group "users")
                 (supplementary-groups '("wheel"
                                         "audio" "video"))
                 (home-directory "/home/alice"))
                (user-account
                 (name "happy")
                 (uid 1001)
                 (comment "Happy")
                 (group "users")
                 (supplementary-groups '("wheel"))
                 (home-directory "/home/happy"))
                %base-user-accounts))

  (packages (cons* nss-certs
                   font-liberation
                   font-dejavu
                   angband-nonfree
                   %base-packages))
  
  (services (cons* (xfce-desktop-service)
                   %custom-desktop-services))

  (name-service-switch %mdns-host-lookup-nss))
