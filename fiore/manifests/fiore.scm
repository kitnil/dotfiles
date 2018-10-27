(use-modules (gnu)
             (gnu packages mtools)
             (guix packages)
             (guix profiles))

(use-package-modules admin android bash bootloaders certs cryptsetup
cups databases dns file fonts fontutils freedesktop gnome gnupg linux
lisp mail monitoring ncurses networking pulseaudio ratpoison readline
rsync screen ssh tmux version-control virtualization web wget xdisorg
xorg zile)

(define %fonts-packages
  (list font-awesome
        font-dejavu
        font-liberation
        font-misc-misc  ;for `xterm'
        font-wqy-zenhei ;Chinese, Japanese, Korean
        ))

(define %icons-packages
  (list adwaita-icon-theme hicolor-icon-theme))

(packages->manifest
  (append (list fuse-exfat
                exfat-utils

                ;; For helm-stumpwm-commands and stumpish
                desktop-file-utils
                dovecot
                gvfs
                rlwrap
                setxkbmap   ; Keyboard layout
                wmctrl      ; `ewmctrl'
                xclip       ; X clipboard CLI
                xdg-utils
                xdotool     ; Mouse and keyboard automation
                xdpyinfo
                xhost
                xlsfonts
                xorg-server ; `xephyr'
                xprop
                xrandr      ; Change screen resolution
                xrdb
                xset
                xsetroot
                xterm       ; $TERM
                xwininfo    ; X Window information
                xmodmap

                adb       ; For Replicant (Android distribution) control
                bridge-utils
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

                fontconfig      ; `fc-cache -f'

                alsa-utils
                pavucontrol ; Pulseaudio control GUI
                pulseaudio

                httpd
                mariadb
                cryptsetup

                sbcl
                sbcl-stumpwm

                zabbix-server)

          %fonts-packages
          %icons-packages
          %base-packages))

