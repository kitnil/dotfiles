(use-modules (gnu)
             (guix packages)
             (guix profiles))

(use-package-modules admin algebra aspell audio backup bittorrent
cdrom chromium ci cmake code commencement compression cpio cran curl
databases dictionaries dns dunst file elf games gcc gdb ghostscript
golang gl glib gnome gnu-doc gnupg gnuzilla graphics graphviz
gstreamer gtk guile guile-xyz haskell image-viewers imagemagick
inkscape kodi libreoffice license linux lisp logging lsof lxde m4 mail
man maths messaging ncdu ncurses networking node package-management
parallel password-utils patchutils pdf perl perl-web php pulseaudio
python python-xyz qt readline rdesktop rsync samba scheme screen
shellutils ssh statistics sqlite suckless synergy tex texinfo
textutils tmux tls tor valgrind version-control video virtualization
w3m web wget xdisorg xml xorg)

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
  (list libreoffice python-pyqt-without-qtwebkit))

(define %spelling-packages
  (list aspell aspell-dict-en aspell-dict-ru))

(packages->manifest
 (append (list feh         ; Image viewer
               ffmpeg      ; Video, audio, images, gif conversion
               imagemagick ; Pipe to `display'

               ;; See <https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424>.
               at-spi2-core
               ghostscript/x gnuplot
               ghc-pandoc  ; Convert Markdown

               jq

               cloc            ; Count code
               direnv
               gnu-make        ; GNU Make
               recutils  ; Filter records like in `guix --search'
               stow            ; Dotfiles management
               the-silver-searcher
               woof

               translate-shell ; Translation in CLI and Emacs
               password-store  ; Password management

               cuirass

               artanis guile-2.2 guile-colorized guile-daemon 
               guile-fibers guile-gcrypt guile-git guile-readline
               guile-redis guile-ssh guile-xosd

               chicken go m4 racket perl python python-hy r sbcl

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
               icecat ungoogled-chromium

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

               tmux
               fbcat

               gsettings-desktop-schemas
               glib-networking
               flatpak
               nix

               mlt mpv obs vlc

               ;; PDF
               zathura zathura-djvu zathura-pdf-mupdf

               ;; X11
               keynav rofi st xauth xev scrot xsel

               perl-uri-escape ;convert url

               patchelf ;patch elf

               octave

               htop inxi iotop jnettop python-glances

               synergy

               ansible

               ;; FAIL: ansible         ; Configuration management
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
               
               reptyr
               shellcheck
               sipcalc
               socat
               sshpass
               sqlite unzip zip
               curl wget

               texinfo

               lvm2 cdrkit-libre qemu samba ubridge virt-manager

               python-pyqt-without-qtwebkit

               tmux tmuxifier parallel w3m znc

               gnu-c-manual gnu-standards man-pages
               sicp ; Structure and Interpretation of Computer Programs

               ;; XXX: Broken package
               ;; restic ;backup

               cflow         ;C program call map.
               gcc-toolchain ;For Emacs `semantic-mode'.
               gdb           ;GNU debuger.
               global        ;Source tagging.
               valgrind      ;Memory debug.

               mesa mesa-utils

               gst-plugins-base gst-plugins-bad gst-plugins-good
               gst-plugins-ugly gstreamer

               minetest ; FOSS Minecraft like game
               )

         guix-collection-packages-multiout
         %large-packages
         %spelling-packages))
