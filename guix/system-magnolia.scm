(use-modules (gnu)
	     (gnu system nss))

(use-service-modules ssh
		     desktop
		     xorg
		     cups
		     pm
		     version-control
		     admin
		     mcron
                     mail)

(use-package-modules bootloaders
		     emacs
		     cups
		     wm
		     certs
		     fonts
                     xdisorg
		     cryptsetup
                     ssh
		     guile
		     package-management
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
   ;; Disable substitutes altogether.
   ;; (substitute-urls '())
   ;; (authorized-keys '())
   (max-silent-time 7200)
   (timeout (* 4 max-silent-time))

   (extra-options '("--max-jobs=6" "--cores=3"
                    "--cache-failures"
                    "--gc-keep-outputs" "--gc-keep-derivations"))))

(define %custom-desktop-services
  (modify-services %desktop-services
    (guix-service-type config => %guix-daemon-config)
    (special-files-service-type config => `(("/bin/sh" ,(file-append bash "/bin/sh"))
					    ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))
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
		       (file-system
                        (device "magnolia-data")
                        (title 'label)
                        (mount-point "/srv")
                        (type "ext4"))
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
		   guile-2.2
		   guix
		   nss-certs
                   font-dejavu
		   font-liberation
                   guile-ssh
		   openssh
		   %base-packages))

  (services (cons* (service openssh-service-type
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
		   (service mcron-service-type)
		   (service rottlog-service-type)
		   %custom-desktop-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
