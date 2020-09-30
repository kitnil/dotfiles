(use-modules (gnu))

(use-package-modules admin base certs cryptsetup docker file linux lisp
suckless xdisorg xorg fonts android fontutils gnome freedesktop readline
ncurses networking pulseaudio wm vnc ssh bittorrent audio lxde version-control
lisp-xyz)

(use-modules (wigust services nix)
             (wigust services autossh)
             (wigust services kresd)
             (wigust services jenkins)
             (wigust services tftp)
             (wigust packages lisp)
             (wigust packages python)
             (majordomo packages majordomo))

(list sbcl stumpwm-checkout `(,stumpwm-checkout "lib")

      sbcl-stumpwm-checkout-ttf-fonts
      sbcl-stumpwm-checkout-globalwindows
      sbcl-stumpwm-checkout-swm-gaps
      sbcl-stumpwm-checkout-stumptray
      sbcl-slime-swank
      stumpish

      ncurses

      fontconfig font-awesome font-dejavu font-liberation
      font-misc-misc font-wqy-zenhei

      gnome-themes-standard adwaita-icon-theme hicolor-icon-theme
      lxappearance

      desktop-file-utils xrdb xset xsetroot xkill
      ;; gvfs depends on webkitgtk

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
      majordomo-ca

      fping

      adb

      iptables bridge-utils

      docker-cli docker-compose

      singularity

      cryptsetup

      pulseaudio

      libcgroup

      openssh ;autofs
      sshfs ;autofs
      fuse ;mount -t fuse and autofs

      file
      iftop
      net-tools
      tcpdump)
