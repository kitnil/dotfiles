(use-modules (gnu))
(use-service-modules networking linux ssh)
(use-package-modules certs curl linux tmux)

(operating-system
  (host-name "gnu")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/vda")))
  (mapped-devices (list (mapped-device
                         (source "vg0")
                         (targets '("vg0-guix"))
                         (type lvm-device-mapping))))
  (file-systems (append (list (file-system
                                (device "/dev/mapper/vg0-guix")
                                (mount-point "/")
                                (dependencies mapped-devices)
                                (type "ext4")))
                        %base-file-systems))
  (users (append (list (user-account
                        (name "user")
                        (comment "Unprivileged user")
                        (group "users")
                        (supplementary-groups '("wheel" "audio" "video"))
                        (password (crypt "password" "$6$abc")))
                       (user-account (inherit %root-account)
                                     (password (crypt "password" "$6$abc"))))
                 %base-user-accounts))
  (initrd-modules (append (list "dm-thin-pool" "dm-snapshot")
                          %base-initrd-modules))
  (packages (append (list curl nss-certs lvm2 tmux)
                    %base-packages))
  (services (append (list (service dhcp-client-service-type)
                          (service openssh-service-type
                                   (openssh-configuration
                                    (permit-root-login #t)
                                    (password-authentication? #t)
                                    (authorized-keys
                                     (let ((ssh-public-key (plain-file "id_rsa.pub" "\
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDEmkOCBXHo6e3IixgJNflxxLDPaLakMWZRGq6qFuqIXPMyr1abLezPrr2Fk8+j8uZr5DyW/vbSs4uTZTuYJ2FXD0s8nZqLnO3eel7YN3YiHD4BxGR7KFSvQVVM7eJ9/RINDp1qZ6uFtGH//vhjqK/rzsysxQFL6sW88mL6sQZd46LQ7Grm1IPkiZALD7/Z3EYpHrcRjf0pN3dkpOjCgLrl0+AuMN6mqffqlJ/hha7xiEJ0w3gSGDJAor/huYZf331cUoQOZOyNfhL8ux5GQGLPMh+371Ilh5UFFksY5hs46jkt16SegHwj6MmGoiBFM9TBqapPzGKkBRUVaGeqg+dtsI/k/XwH1HYd9nF4lZri5GAGRGxpFV/gv6oex/8q7CUlm5zKY7nH7GV2gLaVMCvXqezzqrQJz//IeKPOCcL6MLIbJfRz1eXsNnywUV8Vn6zQo4y6D0VyqwMHzV0JQU74wn2LIPjguIq5iw1GrnMUGpwfm6rAx1/fLWl4uGGssNIJd+JiH8Fo9VLulYLQNgbE5rlbPAvhuOQEIz+T9Gnv9NI5TG7LoywlDjAOSuiOQgdUIr9XlKviRIAeeIkFNkdCLF/xpqkDyRM3DidrNkZno964gxQFe7UlCR/icGUvLyNxzeWfSmrYPwIHK/ntVRkvDGYlKOqAhROesL/X3gUbMQ==")))
                                       `(("root" ,ssh-public-key)
                                         ("user" ,ssh-public-key))))))
                          (service kernel-module-loader-service-type
                                   '("dm-snapshot" "dm-thin-pool")))
                    (modify-services %base-services
                      (guix-service-type config => (guix-configuration
                                                    (substitute-urls '("https://ci.guix.gnu.org"
                                                                       "https://guix.wugi.info"))
                                                    (authorized-keys (append (list (plain-file "guix.wugi.info.pub" "\
(public-key
 (ecc
  (curve Ed25519)
  (q #45BD5CF39730F811FCFDEBB3FA30277DCA7D443430264FB6D48DCEEFE2E23CF5#)
  )
 )
"))
                                                                             %default-authorized-guix-keys)))))))
  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
user ALL=(ALL) NOPASSWD: ALL\n")))
