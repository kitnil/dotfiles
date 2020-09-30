(define-module (config)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages file)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages android)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages lxde)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (packages admin)
  #:use-module (packages lisp)
  #:use-module (packages python)
  #:use-module (packages web)
  #:use-module (packages majordomo)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-26)
  #:export (20-intel.conf
            %my-system-packages
            %my-setuid-programs
            %nginx-deploy-hook
            letsencrypt-certificate
            letsencrypt-key))

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

(define %my-system-packages
  (cons* sbcl stumpwm-checkout `(,stumpwm-checkout "lib")

         sbcl-stumpwm-checkout-ttf-fonts
         sbcl-stumpwm-checkout-globalwindows
         sbcl-stumpwm-checkout-swm-gaps
         sbcl-stumpwm-checkout-stumptray
         sbcl-slime-swank
         stumpish

         ncurses

         fontconfig font-awesome font-dejavu font-liberation
         font-misc-misc font-wqy-zenhei
         font-google-noto ;emoji in chromium

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
         tcpdump

         %base-packages))

(define %my-setuid-programs
  (cons* (file-append fping "/sbin/fping")
         (file-append mtr "/sbin/mtr")
         (file-append ubridge "/bin/ubridge")
         (file-append iputils "/bin/ping")
         (delete (file-append inetutils "/bin/ping6")
                 (delete (file-append inetutils "/bin/ping")
                         %setuid-programs))))

(define letsencrypt-certificate
  (cut string-append "/etc/letsencrypt/live/" <> "/fullchain.pem"))

(define letsencrypt-key
  (cut string-append "/etc/letsencrypt/live/" <> "/privkey.pem"))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

