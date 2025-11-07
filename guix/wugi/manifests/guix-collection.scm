(define-module (wugi manifests guix-collection)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages chicken)
  #:use-module (gnu packages code)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dictionaries)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages entr)
  #:use-module (gnu packages file)
  #:use-module (gnu packages freeipmi)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnu-doc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages kodi)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages minetest)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages php)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pv)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ratpoison)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages synergy)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages telephony)
  #:export (%guix-collection-manifest))

(define (%guix-collection-manifest)
  (define guix-collection-packages-multiout
    `((,glib "bin")
      (,bind "utils")
      (,git "gui")
      (,git "send-email")
      (,git "svn")
      (,guile-3.0 "debug")
      (,alsa-plugins "pulseaudio")
      (,isc-bind "utils")))

  (define %large-packages
    (list libreoffice
          nyxt))

  (define %spelling-packages
    (list aspell aspell-dict-en aspell-dict-ru))

  (packages->manifest
   (append (list guile-bash

                 feh sxiv    ; Image viewers
                 perl-image-exiftool

                 looking-glass-client

                 qemu

                 ffmpeg      ; Video, audio, images, gif conversion
                 imagemagick ; Pipe to `display'

                 xfce leafpad ratpoison gimp

                 ;; kdeconnect

                 dialog
                 entr

                 ;; See <https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424>.
                 at-spi2-core
                 ghostscript/x gnuplot
                 ;; grip ; TODO: Fix conflicting entries for python-cryptography

                 autojump
                 jc jq jo jtbl
                 nmap
                 ;; hosts ;TODO: Fix build failure.
                 hss

                 dmidecode
                 lm-sensors

                 cloc            ; Count code
                 diffstat
                 direnv
                 gnu-make        ; GNU Make
                 recutils  ; Filter records like in `guix --search'
                 stow            ; Dotfiles management
                 the-silver-searcher
                 fd ripgrep
                 woof

                 zsh

                 dragon-drop

                 multipath-tools
                 ;; kpartx
                 ;; [[https://dev.to/c33s/how-to-mount-a-lvm-volume-which-is-inside-a-lvm-volume-jnl]
                 ;;  [How to mount a lvm volume which is inside a lvm volume - DEV Community]]

                 gnu-gettext
                 translate-shell ; Translation in CLI and Emacs

                 ;; TODO: Fix cuirass

                 ;; TODO: Fix artanis
                 guile-colorized guile-daemon
                 guile-fibers guile-gcrypt guile-git guile-readline
                 guile-redis guile-ssh guile-xosd guile-wisp

                 chicken go m4 perl python ;; python-hy
                 r sbcl ghc

                 mcron

                 git tig mercurial gource vc-dwim git-cal gita

                 colormake

                 guile-commonmark ; Commonmark for Guile
                 ;; gwl              ; Guix workflow management ; fails to build
                 haunt            ; Guile static site generator

                 aria2        ; Download utility
                 ;; kodi
                 kodi-cli     ; Remote control Kodi
                 ;; transmission ; Bittorrent
                 qbittorrent

                 mps-youtube
                 streamlink
                 twitchy
                 youtube-dl   ; Video and music from websites

                 redshift  ; Color temperature
                 neofetch

                 iperf

                 xxd

                 ;; python-clf ; Interface to <https://commandlinefu.com/>

                 dbus dunst xmessage libnotify

                 alsa-utils cli-visualizer pulsemixer pavucontrol qpwgraph

                 ;; WEB
                 ;; icecat
                 ;; broken: ungoogled-chromium

                 ;; node ;Packages in <~/.npm-global/bin/>.

                 tor torsocks

                 ;; Mail
                 isync msmtp notmuch neomutt swaks
                 ;; IMAP
                 go-gitlab.com-shackra-goimapnotify

                 gnupg pinentry

                 file tree python-pygments exa

                 ;; TODO: Fix python-parso
                 python-language-server

                 ;; mongodb

                 goaccess
                 ;; httpie ; TODO: Fix conflicting entries for python-cryptography
                 httping

                 ;; Utilities
                 strace tcpdump multitail wireshark

                 hdparm

                 ddcutil

                 console-setup ;set font size in TTY
                 rlwrap ; read-line wrapper

                 tmux tmux-xpanes screen
                 fbcat
                 shell-functools

                 gsettings-desktop-schemas
                 glib-networking
                 ;; flatpak
                 ;; nix

                 mlt mpv vlc mpg123 sox

                 v4l-utils

                 ;; PDF
                 ;; zathura zathura-djvu zathura-pdf-mupdf poppler

                 ;; X11
                 keynav rofi st xauth xev scrot xsel xclickroot xmenu
                 kitty

                 ;; polybar

                 sxhkd

                 perltidy ;format Perl code
                 perl-uri-escape ;convert url
                 perl-moose

                 binutils
                 patchelf ;patch elf
                 patchutils

                 ;; octave XXX: Fails to compile.

                 atop htop inxi iotop jnettop glances bmon progress

                 moonlight-qt

                 synergy

                 ansible         ; Configuration management
                 ;; chezmoi XXX: Fails to compile.

                 bc
                 cpio
                 detox ; Replace spaces with underscores in filenames
                 dos2unix
                 freerdp
                 graphviz ;produce `dot' files graphs
                 xdot
                 html-xml-utils

                 ;; XXX: Fails to compile licensecheck ; Licence checker for source files

                 lsof
                 lsofgraph

                 fatrace
                 ncdu            ; TUI `du'
                 sysstat
                 netcat-openbsd
                 openssl
                 nethogs
                 rsync

                 picom

                 php
                 
                 autossh
                 sshuttle

                 reptyr
                 ;; shellcheck
                 ipcalc
                 socat
                 sshpass
                 sshfs
                 sqlite unzip zip p7zip
                 curl wget
                 iputils

                 texinfo

                 lvm2 cdrkit-libre samba ubridge

                 python-pymongo
                 python-evdev

                 mariadb redis

                 tmux tmuxifier parallel w3m
                 ;; znc

                 source-highlight

                 gnu-c-manual gnu-standards man-pages
                 sicp ; Structure and Interpretation of Computer Programs

                 restic ;backup

                 cflow         ;C program call map.
                 gcc-toolchain ;For Emacs `semantic-mode'.
                 gdb           ;GNU debuger.
                 global        ;Source tagging.
                 valgrind      ;Memory debug.

                 obs-looking-glass

                 mesa mesa-utils

                 gst-plugins-base gst-plugins-bad gst-plugins-good
                 gst-plugins-ugly gstreamer

                 minetest ; FOSS Minecraft like game
                 gimp inkscape

                 openvpn
                 mosh
                 ponymix
                 ;; procmail ; fails because of gnutls-dane
                 pv
                 pwgen
                 syncthing

                 xfce4-screenshooter
                 xfce4-terminal

                 xplanet

                 xmodmap

                 whois

                 bluez

                 password-store ; Password management

                 dconf ;used by virt-manager as a storage for configuration.

                 virt-manager

                 freeipmi

                 mumble)

           guix-collection-packages-multiout
           %large-packages
           %spelling-packages)))
