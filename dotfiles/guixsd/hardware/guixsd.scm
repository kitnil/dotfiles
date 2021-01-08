(use-modules (gnu))
(use-service-modules networking)

(operating-system
  (host-name "guixsd")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sda")))

  (file-systems (cons* (file-system
                         (device (file-system-label "guixsd-root"))
                         (mount-point "/")
                         (type "ext4"))
                       (file-system
                         (device (file-system-label "magnolia-data"))
                         (mount-point "/srv")
                         (type "ext4"))
                       (file-system
                         (device "tmpfs")
                         (mount-point "/tmp")
                         (type "tmpfs")
                         (check? #f)
                         (flags '(no-dev))
                         (options "mode=1777,size=50%"))
                       %base-file-systems))

  (swap-devices '("/dev/disk/by-label/guixsd-swap"))

  (packages %base-packages)

  (services (cons (service dhcp-client-service-type)
		  %base-services)))
