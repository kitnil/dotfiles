(use-modules (gnu)
	     (gnu system nss)
	     (linux-nonfree))

(use-service-modules ssh
		     desktop
		     xorg
		     cups
		     pm)

(use-package-modules bootloaders
		     admin
		     emacs
		     cups
		     wm
		     certs
		     fonts
		     xdisorg
		     cryptsetup)

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

  (services (cons* (service tlp-service-type)
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
		   %desktop-services)))
