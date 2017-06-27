(use-modules (gnu)
	     (gnu system nss))

(use-service-modules ssh
		     desktop
		     xorg
		     cups)

(use-package-modules bootloaders
		     emacs
		     cups
		     wm
		     certs
		     fonts
                     xdisorg
		     cryptsetup)

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

(define %custom-desktop-services
  (modify-services %desktop-services
		   (slim-service-type config => (slim-configuration
						 (inherit config)
						 (startx
						  (xorg-start-command
						   #:configuration-file
						   (xorg-configuration-file
						    #:extra-config (list 20-intel.conf))))))))

(operating-system
  (host-name "magnolia")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (grub-configuration (grub grub-efi)
				  (device "/dev/sda")))

  (file-systems (cons* (file-system
                        (device "magnolia-root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                       (file-system
                        (device "/dev/sda1")
                        (mount-point "/boot/efi")
                        (type "vfat"))
		       %base-file-systems))

  (users (cons (user-account
                (name "natsu")
		(uid 1000)
                (comment "Oleg Pykhalov")
                (group "users")

                (supplementary-groups '("wheel"
                                        "audio"
					"video"
					"lpadmin"
					"lp"))
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
		   %base-packages))

  (services (cons* (service openssh-service-type
			    (openssh-configuration
			     (port-number 22)))
		   (service cups-service-type
			    (cups-configuration
			     (web-interface? #t)
			     (extensions
			      (list cups-filters hplip))))
		   %custom-desktop-services)))
