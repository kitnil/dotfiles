(use-modules (gnu)
             (majordomo packages majordomo)
             (guix gexp)
             (wigust packages lisp)
             (services autossh))

(use-package-modules admin base certs cryptsetup docker linux lisp
                     suckless xdisorg xorg fonts android fontutils
                     gnome freedesktop readline ncurses networking)

(use-service-modules desktop networking ssh xorg)

(define %system
  (operating-system
    (host-name "workstation-guixsd")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sda")))

    (users (cons (user-account (name "oleg")
                               (comment "Oleg Pykhalov")
                               (group "users")
                               (supplementary-groups '("wheel" "audio" "video")))
                 %base-user-accounts))

    (file-systems (cons* (file-system
                           (device (uuid "11d541d4-7914-4937-8ce0-7e50687ddbc6"))
                           (mount-point "/")
                           (type "ext4"))
                         (file-system
                           (device "tmpfs")
                           (mount-point "/tmp")
                           (type "tmpfs")
                           (check? #f)
                           (flags '(no-dev))
                           (options "mode=1777,size=50%"))
                         %base-file-systems))

    (packages (cons* nss-certs ;SSL certificates
                     majordomo-ca

                     sbcl stumpwm-checkout `(,stumpwm-checkout "lib")

                     ncurses

                     fontconfig font-dejavu

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

                     iptables bridge-utils
                     cryptsetup

                     %base-packages))

    (services (cons* (service openssh-service-type)
                     (service autossh-service-type
                              (autossh-configuration
                               (openssh-client-config
                                (openssh-client-configuration
                                 (hosts (list (openssh-client-host-configuration
                                               (host "guix.duckdns.org")
                                               (identity-file "/etc/autossh/id_rsa")
                                               (strict-host-key-checking? #f)
                                               (user "majordomo-ssh-tunnel")
                                               (user-known-hosts-file "/dev/null")
                                               (extra-options
                                                "
RemoteForward 9999 localhost:22
RemoteForward 16050 127.0.0.1:15050
Compression yes
ExitOnForwardFailure yes
ServerAliveInterval 30
ServerAliveCountMax 3"))))))
                               (host "guix.duckdns.org")))
                     %desktop-services))

    (setuid-programs (cons* (file-append fping "/sbin/fping")
                            (file-append mtr "/sbin/mtr")
                            %setuid-programs))

    (sudoers-file (local-file "sudoers"))))

;; %system

(list (machine
       (operating-system %system)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "172.16.100.60")
                       (system "x86_64-linux")
                       (user "oleg")
                       (identity "/home/oleg/.ssh/id_rsa")))))
