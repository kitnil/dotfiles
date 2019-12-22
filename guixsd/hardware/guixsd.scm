(use-modules (gnu))
(use-service-modules networking)

(operating-system
  (host-name "guixsd")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (target "/boot/efi")))

  (file-systems (cons* (file-system
                         (device (file-system-label "guixsd-root"))
                         (mount-point "/")
                         (type "ext4"))
                       (file-system
                         (device (uuid "A2C0-99E9" 'fat))
                         (mount-point "/boot/efi")
                         (type "vfat"))
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

  (swap-devices '("/dev/disk/by-label/guixsd-swap") ;; '("/dev/sda3")
                )
  
  (packages %base-packages)

  (services (cons (static-networking-service "enp6s0" "192.168.105.120"
					     #:netmask "255.255.255.0"
					     #:gateway "192.168.105.1"
					     #:name-servers '("127.0.0.1\nsearch intr majordomo.ru"
                                                              "80.80.80.80"
                                                              "80.80.81.81"))
		  %base-services)))
