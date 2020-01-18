(use-modules (gnu)
             (majordomo packages majordomo)
             (guix gexp)
             (wigust packages lisp)
             (wigust services autossh)
             (wigust services nix)
             (srfi srfi-1)
             (srfi srfi-26))

(use-package-modules admin base certs cryptsetup docker linux lisp
                     suckless xdisorg xorg fonts android fontutils
                     gnome freedesktop readline ncurses networking)

(use-service-modules desktop dbus monitoring networking ssh xorg)

(define 20-intel.conf "\
# Fix tearing for Intel graphics card.
# Origin: https://wiki.archlinux.org/index.php/Intel_Graphics
#         https://github.com/8p8c/my-guix/blob/master/config.scm
Section \"Device\"
   Identifier  \"Intel Graphics\"
   Driver      \"intel\"
   Option      \"AccelMethod\"  \"sna\"
   Option      \"SwapbuffersWait\" \"true\"
   Option      \"TearFree\" \"true\"
EndSection\n")

(define 30-multihead.conf "\
Section \"Monitor\"
    Identifier  \"HDMI-1\"
    Option      \"Primary\" \"true\"
EndSection

Section \"Monitor\"
    Identifier  \"VGA-1\"
    Option      \"RightOf\" \"HDMI1\"
EndSection")

(define %slim-theme
  (or (and=> (current-filename)
             (lambda (file)
               (string-append (dirname (dirname file))
                              "/fiore/modules/slim-artwork.scm")))
      "/home/oleg/src/dotfiles/fiore/modules/slim-artwork.scm"))

(operating-system
  (host-name "workstation-guixsd")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sdb")))

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
                         (device (file-system-label "workstation-guix"))
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

  (swap-devices '("/dev/disk/by-uuid/fdaef2e9-eda2-48d9-80f8-3d6551ee15fb"))

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
                             (autossh-client-config
                              (autossh-client-configuration
                               (hosts (list (autossh-client-host-configuration
                                             (host "guix.duckdns.org")
                                             (identity-file "/etc/autossh/id_rsa")
                                             (strict-host-key-checking? #f)
                                             (user "majordomo-ssh-tunnel")
                                             (user-known-hosts-file "/dev/null")
                                             (extra-options
                                              "
RemoteForward 0.0.0.0:9999 localhost:22
RemoteForward 0.0.0.0:16050 127.0.0.1:10050
LocalForward 0.0.0.0:5901 127.0.0.1:5901
LocalForward 0.0.0.0:5902 127.0.0.1:5902
Compression yes
ExitOnForwardFailure yes
ServerAliveInterval 30
ServerAliveCountMax 3"))))))
                             (host "guix.duckdns.org")))
                   (service zabbix-agent-service-type)
                   nix-service

                   ;; Desktop services
                   (service slim-service-type
                            (slim-configuration
                             (theme %slim-theme)
                             (xorg-configuration
                              (xorg-configuration
                               (extra-config (list 20-intel.conf
                                                   30-multihead.conf))))))
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

                   (static-networking-service "enp3s0" "172.16.100.60"
                                              #:netmask "255.255.255.0"
                                              #:gateway "172.16.100.3"
                                              #:name-servers '("172.16.100.3\nsearch intr majordomo.ru"
                                                               "172.16.102.2"
                                                               "172.16.103.2"))
                   

                   (modify-services %base-services
                     (guix-service-type config => (guix-configuration
                                                   (substitute-urls '("https://ci.guix.gnu.org" "https://guix.duckdns.org")))))))

  (setuid-programs (cons* (file-append fping "/sbin/fping")
                          (file-append mtr "/sbin/mtr")
                          %setuid-programs))

  (sudoers-file (local-file "sudoers")))
