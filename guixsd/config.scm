(use-modules (gnu))
(use-package-modules base certs lisp suckless xdisorg xorg fonts fontutils gnome freedesktop readline)
(use-service-modules dbus desktop networking sound xorg ssh)

(define 20-intel.conf "\
# This block fixes tearing on Intel GPU.
# Origin: https://wiki.archlinux.org/index.php/Intel_Graphics
#         https://github.com/8p8c/my-guix/blob/master/config.scm
Section \"Device\"
   Identifier  \"Intel Graphics\"
   Driver      \"intel\"
   Option      \"AccelMethod\"  \"sna\"
   Option      \"SwapbuffersWait\" \"true\"
   Option      \"TearFree\" \"true\"
EndSection\n")

(let ((base-system (load "/etc/config.scm")))
  (operating-system (inherit base-system)
		    (packages (cons* stumpwm

				    fontconfig
				    font-awesome
				    font-dejavu
				    font-liberation
				    font-misc-misc
				    font-wqy-zenhei

				    adwaita-icon-theme
				    hicolor-icon-theme

				    desktop-file-utils
				    gvfs
				    setxkbmap   ; Keyboard layout
				    wmctrl      ; `ewmctrl'
				    xclip       ; X clipboard CLI
				    xdg-utils
				    xdotool     ; Mouse and keyboard automation
				    xorg-server ; `xephyr'
				    xrandr      ; Change screen resolution
				    xrdb
				    xset
				    xsetroot
				    xterm       ; $TERM
				    xwininfo    ; X Window information
				    ;; For helm-stumpwm-commands and stumpish
				    rlwrap
				    xprop
				    xhost
                                    xkill

                                    nss-certs

				    (operating-system-packages base-system)))
    (users (cons* (user-account
                   (name "oleg")
                   (uid 1000)
                   (comment "Oleg Pykhalov")
                   (group "users")
                   (supplementary-groups '("wheel" "audio" "video"))
                   (home-directory "/home/oleg"))
		  (user-account
                   (name "majordomo-ssh-tunnel")
                   (group "users")
                   (comment "SSH forwarding privilege separation user")
                   (home-directory "/home/majordomo-ssh-tunnel"))
            %base-user-accounts))

    (services (cons* (extra-special-file "/usr/bin/env"
		    			 (file-append coreutils "/bin/env"))
		     (service slim-service-type
		    	      (slim-configuration
			       (startx
				(xorg-start-command
				 #:configuration-file (xorg-configuration-file
						       #:extra-config (list 20-intel.conf))))))
		     (screen-locker-service slock)
		     (screen-locker-service xlockmore "xlock")
		     (udisks-service)
		     (upower-service)
		     (accountsservice-service)
		     (colord-service)
		     (geoclue-service)
		     (service polkit-service-type)
		     (elogind-service)
		     (dbus-service)
		     (service ntp-service-type)
		     x11-socket-directory-service
		     (service alsa-service-type)

		     (service openssh-service-type
			      (openssh-configuration
			       (x11-forwarding? #t)
			       (password-authentication? #f)))

		     (operating-system-user-services base-system)))))
