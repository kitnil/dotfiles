(use-modules (gnu)
             (guix packages)
             (guix profiles))

(use-package-modules admin algebra aspell audio backup bittorrent
cdrom chromium ci cmake code commencement compression cpio cran curl
databases dictionaries diffoscope dns dunst file elf entr games gcc
gdb gimp ghostscript golang gl glib gnome gnu-doc gnupg gnuzilla
graphics graphviz gstreamer gtk guile guile-xyz haskell haskell-apps
haskell-xyz image-viewers imagemagick inkscape irc kde kodi
libreoffice license linux lisp logging lsof lxde m4 mail man maths
messaging ncdu ncurses networking node package-management parallel
password-utils patchutils pdf perl perl-web php pretty-print
pulseaudio pv python python-xyz qt ratpoison readline rdesktop rsync
samba scheme screen shellutils ssh statistics sqlite suckless
syncthing synergy tex texinfo textutils text-editors tigervnc tmux tls
tor valgrind version-control video virtualization vpn w3m xfce web
wget wm xdisorg xml xorg)

(define guix-collection-packages-multiout
  `((,glib "bin")
    ;; (,bind "utils")
    (,git "gui")
    (,git "send-email")
    (,git "svn")
    (,guile-2.2 "debug")
    (,alsa-plugins "pulseaudio")
    (,isc-bind "utils")))

(define %large-packages
  (list ;; libreoffice
        python-pyqt-without-qtwebkit))

(define %spelling-packages
  (list aspell aspell-dict-en aspell-dict-ru))

(packages->manifest
 (append (list feh sxiv    ; Image viewers

               ffmpeg      ; Video, audio, images, gif conversion
               imagemagick ; Pipe to `display'

               xfce leafpad ratpoison gimp

               kdeconnect

               entr

               ;; See <https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424>.
               at-spi2-core
               ghostscript/x gnuplot
               ghc-pandoc  ; Convert Markdown

               autojump
               jq
               nmap

               cloc            ; Count code
               direnv
               gnu-make        ; GNU Make
               recutils  ; Filter records like in `guix --search'
               stow            ; Dotfiles management
               the-silver-searcher
               woof

               translate-shell ; Translation in CLI and Emacs
               password-store  ; Password management
               rofi-pass       ; front-end to password-store

               cuirass

               artanis guile-2.2 guile-colorized guile-daemon 
               guile-fibers guile-gcrypt guile-git guile-readline
               guile-redis guile-ssh guile-xosd

               chicken go m4 racket perl python ;; python-hy
               r sbcl ghc

               mcron

               git colordiff mercurial gource

               colormake

               guile-commonmark ; Commonmark for Guile
               ;; gwl              ; Guix workflow management ; fails to build
               haunt            ; Guile static site generator

               aria2        ; Download utility
               kodi-cli     ; Remote control Kodi
               transmission ; Bittorrent

               mps-youtube
               streamlink
               twitchy
               youtube-dl   ; Video and music from websites

               redshift  ; Color temperature
               neofetch

               python-clf ; Interface to <https://commandlinefu.com/>

               dbus dunst xmessage libnotify

               alsa-utils cli-visualizer pulsemixer pavucontrol

               ;; WEB
               ;; icecat
               ;; broken: ungoogled-chromium

               node ;Packages in <~/.npm-global/bin/>.

               tor torsocks

               ;; Mail
               isync msmtp notmuch

               gnupg pinentry

               file tree python-pygments

               ;; Utilities
               strace tcpdump multitail wireshark

               hdparm

               console-setup ;set font size in TTY
               rlwrap ; read-line wrapper

               tmux tmux-xpanes screen
               fbcat

               gsettings-desktop-schemas
               glib-networking
               flatpak
               ;; nix

               mlt mpv vlc

               ;; PDF
               zathura zathura-djvu zathura-pdf-mupdf

               ;; X11
               keynav rofi st xauth xev scrot xsel

               ;; polybar

               sxhkd

               perl-uri-escape ;convert url

               patchelf ;patch elf

               octave

               htop inxi iotop jnettop python-glances

               synergy

               ansible         ; Configuration management
               bc
               cpio
               detox ; Replace spaces with underscores in filenames
               diffoscope
               dos2unix
               freerdp
               graphviz ;produce `dot' files graphs
               html-xml-utils
               licensecheck ; Licence checker for source files
               lsof
               ncdu            ; TUI `du'
               netcat          ; TCP nmap
               openssl
               nethogs
               rsync

               php
               
               autossh
               sshuttle

               reptyr
               shellcheck
               sipcalc
               socat
               sshpass
               sshfs
               sqlite unzip zip
               curl wget

               texinfo

               lvm2 cdrkit-libre samba ubridge virt-manager

               python-pyqt-without-qtwebkit

               tmux tmuxifier parallel w3m znc

               source-highlight

               gnu-c-manual gnu-standards man-pages
               sicp ; Structure and Interpretation of Computer Programs

               restic ;backup

               cflow         ;C program call map.
               gcc-toolchain ;For Emacs `semantic-mode'.
               gdb           ;GNU debuger.
               global        ;Source tagging.
               valgrind      ;Memory debug.

               mesa mesa-utils

               gst-plugins-base gst-plugins-bad gst-plugins-good
               gst-plugins-ugly gstreamer

               minetest ; FOSS Minecraft like game
               gimp inkscape

               openvpn
               mosh
               ponymix
               procmail
               pv
               pwgen
               syncthing
               quassel

               tigervnc-server
               tigervnc-client

               xfce4-screenshooter
               xfce4-terminal)

         guix-collection-packages-multiout
         %large-packages
         %spelling-packages))
