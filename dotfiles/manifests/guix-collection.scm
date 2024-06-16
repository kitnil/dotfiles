(use-modules (gnu)
             (guix packages)
             (guix profiles))

(use-package-modules admin algebra aspell astronomy audio backup bash
bittorrent cdrom chicken chromium ci cmake code commencement compression
compton configuration-management cpio cran curl databases dictionaries
diffoscope disk dns elf entr file freeipmi games gettext gcc gdb ghostscript
gimp gl glib gnome gnu-doc gnupg gnuzilla golang graphics graphviz gstreamer
gtk guile guile-xyz haskell haskell-apps haskell-xyz imagemagick image-viewers
inkscape irc kde kodi libreoffice license linux lisp logging lsof lxde lxqt m4
mail man maths messaging minetest mp3 ncdu ncurses networking node
package-management parallel password-utils patchutils pdf perl perl-web php
pretty-print photo pulseaudio pv python python-web python-xyz qt ratpoison
rdesktop readline rsync rust-apps samba scheme screen shells shellutils
sqlite ssh statistics suckless syncthing synergy terminals tex texinfo
text-editors telegram textutils tls tmux tor valgrind version-control video
virtualization vnc vim vpn w3m web web-browsers wget wm xdisorg xfce xml xorg)

(define guix-collection-packages-multiout
  `((,glib "bin")
    ;; (,bind "utils")
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

               kmonad

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
               jq jo jtbl
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

               bats
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

               freeipmi)

         guix-collection-packages-multiout
         %large-packages
         %spelling-packages))
