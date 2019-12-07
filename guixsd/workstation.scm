(use-modules (gnu)
             (majordomo packages majordomo)
             (guix gexp)
             (wigust packages lisp)
             (services autossh)
             (services nix)
             (srfi srfi-1)
             (srfi srfi-26))

(use-package-modules admin base certs cryptsetup docker linux lisp
                     suckless xdisorg xorg fonts android fontutils
                     gnome freedesktop readline ncurses networking)

(use-service-modules desktop monitoring networking ssh xorg)

(operating-system
  (host-name "workstation-guixsd")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sda")))

  (groups (cons (user-group (name "nixbld")
                            (id 30100))
                %base-groups))

  (users (cons (user-account (name "oleg")
                             (comment "Oleg Pykhalov")
                             (group "users")
                             (supplementary-groups '("wheel" "audio" "video")))
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

                                     ;; guix-daemon expects GROUP to be listed as a
                                     ;; supplementary group too:
                                     ;; <http://lists.gnu.org/archive/html/bug-guix/2013-01/msg00239.html>.
                                     (supplementary-groups (list group "kvm"))

                                     (comment (format #f "Nix Build User ~a" n))
                                     (home-directory "/var/empty")
                                     (shell (file-append shadow "/sbin/nologin"))))
                                  1+
                                  1))
                        9)
                       %base-user-accounts)))

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

  (swap-devices '("/dev/disk/by-uuid/6e0281d7-abed-4d01-91f3-72481014515a"))

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
RemoteForward 0.0.0.0:9999 localhost:22
RemoteForward 0.0.0.0:16050 127.0.0.1:10050
Compression yes
ExitOnForwardFailure yes
ServerAliveInterval 30
ServerAliveCountMax 3"))))))
                             (host "guix.duckdns.org")))
                   (service zabbix-agent-service-type)
                   nix-service
                   %desktop-services))

  (setuid-programs (cons* (file-append fping "/sbin/fping")
                          (file-append mtr "/sbin/mtr")
                          %setuid-programs))

  (sudoers-file (local-file "sudoers")))
