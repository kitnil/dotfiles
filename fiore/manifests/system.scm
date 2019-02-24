(define-module (fiore manifests system)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages android)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ratpoison)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zile)
  #:export (fiore-packages))

(define fiore-packages
  (list

   desktop-file-utils
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
   iptables
   lm-sensors      ; `sensors'
   nss-certs ; for https
   openssh   ; `scp'
   qemu
   rsync
   sshfs-fuse
   strace
   tcpdump
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

   httpd
   mariadb
   cryptsetup

   ))
