;; GuixSD configuration file for the virtual machine.
;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (gnu) (gnu system nss))
(use-service-modules desktop xorg spice)
(use-package-modules certs gnome bash)

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
                                  (auto-login-session (file-append
                                                       gnome-session
                                                       "/bin/gnome-session"))
                                  (default-user "bob")))))

(operating-system
  (host-name "gnome")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (grub-configuration (device "/dev/sda")))

  (file-systems (cons (file-system
                        (device "my-root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "bob")
                (comment "Alice's brother")
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video"))
                (home-directory "/home/bob"))
               %base-user-accounts))

  (packages (cons* nss-certs
                   gvfs
                   %base-packages))

  (services (cons* (gnome-desktop-service)
                   (spice-vdagent-service)
                   %custom-desktop-services))

  (name-service-switch %mdns-host-lookup-nss))
