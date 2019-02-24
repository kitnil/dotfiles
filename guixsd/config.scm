(use-modules (gnu) (srfi srfi-1) (srfi srfi-26))
(use-package-modules admin base certs lisp suckless xdisorg xorg fonts
                     fontutils gnome freedesktop readline)
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
  (operating-system
    (inherit base-system)
    (packages (cons* stumpwm

		     fontconfig font-awesome font-dejavu font-liberation
                     font-misc-misc font-wqy-zenhei

		     adwaita-icon-theme hicolor-icon-theme

		     desktop-file-utils gvfs xrdb xset xsetroot xkill

		     setxkbmap   ;keyboard layout
		     wmctrl      ;`ewmctrl'
		     xclip       ;X clipboard CLI
		     xdg-utils   ;finds a program to open file
		     xdotool     ;mouse and keyboard automation
		     xorg-server ;`xephyr' for x11 testing
		     xrandr      ;change screen resolution
		     xterm       ;$TERM terminal
		     xwininfo    ;X window information
		     ;; For helm-stumpwm-commands and stumpish
		     rlwrap
		     xprop
		     xhost
                     
                     nss-certs ;SSL certificates

		     (operating-system-packages base-system)))

    (groups (cons* (user-group (name "nixbld")
                               (system? #t))
                   ;; (user-group (name "adbusers"))
                   ;; (user-group (name "guix-offload"))
                   ;; (user-group (name "telegraf") (system? #t))
                   ;; (user-group (name "git") (id 30003))
                   ;; (user-group (name "jenkins") (id 30004))
                   ;; (user-group (name "influxdb") (id 30005))
                   ;; (user-group (name "grafana") (id 30006))
                   (user-group (name "docker")
                               (system? #t))
                   ;; (user-group (name "jenkinsbuild"))
                   ;; (user-group (name "alerta"))
                   %base-groups))

    (users (cons* (user-account
                   (name "oleg")
                   (uid 1000)
                   (comment "Oleg Pykhalov")
                   (group "users")
                   (supplementary-groups '("wheel" "audio" "video" "docker"))
                   (home-directory "/home/oleg"))
		  (user-account
                   (name "majordomo-ssh-tunnel")
                   (group "users")
                   (comment "SSH forwarding privilege separation user")
                   (home-directory "/home/majordomo-ssh-tunnel"))
                  (append ((lambda* (count #:key
                                      (group "nixbld")
                                      (first-uid 30101)
                                      (shadow shadow))
                             (unfold (cut > <> count)
                                     (lambda (n)
                                       (user-account
                                        (name (format #f "nixbld~a" n))
                                        (system? #t)
                                        (uid (+ first-uid n -1))
                                        (group group)
                                        (comment (format #f "Nix Build User ~a" n))
                                        (home-directory "/var/empty")
                                        (shell (file-append shadow "/sbin/nologin"))))
                                     1+
                                     1))
                           9)
                          %base-user-accounts)))

    (services (cons* (extra-special-file "/usr/bin/env"
		    			 (file-append coreutils "/bin/env"))

                     ;; Desktop services
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
