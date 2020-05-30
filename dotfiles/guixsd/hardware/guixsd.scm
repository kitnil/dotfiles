(use-modules (gnu))
(use-service-modules networking)

(operating-system
  (host-name "guixsd")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sdb")))

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

  (services (cons (static-networking-service "enp6s0" "192.168.100.120"
					     #:netmask "255.255.255.0"
					     #:gateway "192.168.100.1"
					     #:name-servers '("192.168.100.120\nsearch intr majordomo.ru"
                                                              "172.17.0.1"
                                                              "8.8.8.8"
                                                              "8.8.4.4"))
		  %base-services)))
