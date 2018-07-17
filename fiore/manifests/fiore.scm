(define-module (fiore manifests fiore)
  #:use-module (gnu)
  #:use-module (gnu packages mtools)
  #:use-module (guix packages)
  #:use-module (guix profiles)
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
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages ratpoison)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
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
  #:export (%fiore-packages
            fiore-fonts-packages
            fiore-icons-packages))

(define fiore-fonts-packages
  (list

   font-awesome
   font-dejavu
   font-liberation
   font-misc-misc  ; for `xterm'
   font-wqy-zenhei ; Chinese, Japanese, Korean

   ))

(define fiore-icons-packages
  (list

   adwaita-icon-theme
   hicolor-icon-theme

   ))

(define %fiore-packages
  (append
   (list

    fuse-exfat
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

    )

   fiore-fonts-packages
   fiore-icons-packages
   %base-packages))
