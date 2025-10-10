;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (wugi manifests pc0-packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dictionaries)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix profiles)

  #:export (%pc0-packages-manifest))

(define (%pc0-packages-manifest)
  (packages->manifest
   (list autojump
         bluez
         clipman
         curl
         ddcutil
         dialog
         direnv
         dmenu
         dtach
         fd
         freerdp
         git
         gnu-make
         gnupg
         htop
         iputils
         jq
         lm-sensors
         looking-glass-client
         moonlight-qt
         mpv
         mtr
         ncdu
         netcat-openbsd
         password-store
         pavucontrol
         pinentry
         pulsemixer
         python
         python-psutil
         qbittorrent
         remmina
         ripgrep
         rsync
         screen
         skopeo
         socat
         sshfs
         sshpass
         strace
         tigervnc-client
         tmux
         translate-shell
         v4l-utils
         virt-manager
         wl-clipboard
         xdot)))
