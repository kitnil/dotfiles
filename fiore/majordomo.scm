(use-modules (gnu))
(use-service-modules cups desktop dns mail networking rsync shepherd
spice ssh version-control web xorg cgit)

(use-package-modules admin android bash bootloaders certs cryptsetup cups
databases dns file fonts fontutils freedesktop gnome gnupg linux mail
ncurses networking ratpoison readline rsync pulseaudio screen ssh tmux
version-control virtualization web wget xdisorg xorg zile)

(operating-system
  (host-name "majordomo")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sda")))
  (file-systems (cons (file-system
                        (device "pyhalov-root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users (cons (user-account
                (name "pyhalov")
                (comment "Oleg Pyhalov")
                (group "users")

                ;; Adding the account to the "wheel" group
                ;; makes it a sudoer.  Adding it to "audio"
                ;; and "video" allows the user to play sound
                ;; and access the webcam.
                (supplementary-groups '("wheel" "audio" "video"))
                (home-directory "/home/pyhalov"))
               %base-user-accounts))

  ;; Globally-installed packages.
  (packages (cons* desktop-file-utils
                   dovecot
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

                   adb       ; For Replicant (Android distribution) control
                   cups      ; Printer
                   ethtool   ; wol (wake on lan)
                   file      ; Information about file from magic
                   gnupg
                   iptables
                   knot
                   lm-sensors      ; `sensors'
                   ncurses
                   nss-certs ; for https
                   openssh   ; `scp'
                   pinentry  ; Password typing for Gnupg
                   qemu
                   rsync
                   sshfs-fuse
                   strace
                   tcpdump
                   tmux
                   tree      ; List files as a tree
                   wget
                   xkill
                   zile

                   adwaita-icon-theme
                   hicolor-icon-theme
                   font-awesome
                   font-dejavu
                   font-liberation
                   font-misc-misc  ; for `xterm'
                   font-wqy-zenhei ; Chinese, Japanese, Korean
                   fontconfig      ; `fc-cache -f'

                   alsa-utils
                   pavucontrol ; Pulseaudio control GUI
                   pulseaudio

                   httpd
                   mysql
                   cryptsetup

                   %base-packages))

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (cons* (service openssh-service-type
                            (openssh-configuration
                             (port-number 2222)))
                   (xfce-desktop-service)
                   %desktop-services)))
