(use-modules (gnu)
             (guix packages)
             (guix profiles))

(use-package-modules admin algebra aspell audio backup bittorrent
cdrom ci cmake code commencement compression cpio cran curl
databases dictionaries dns elf games gcc gdb ghostscript golang gl glib
gnu-doc gnupg gnuzilla graphics graphviz gstreamer gtk guile guile-xyz haskell
image-viewers imagemagick inkscape kodi libreoffice license linux lisp
logging lsof lxde m4 mail man maths messaging ncdu ncurses networking
node package-management parallel password-utils patchutils pdf perl
perl-web python python-xyz qt rdesktop samba scheme screen shellutils
ssh statistics sqlite suckless synergy tex texinfo textutils tmux tls tor
valgrind version-control video virtualization w3m web wget xdisorg
xml xorg)

(define guix-collection-packages-multiout
  `((,glib "bin")
    (,bind "utils")
    (,git "gui")
    (,git "send-email")
    (,git "svn")
    (,guile-2.2 "debug")
    (,alsa-plugins "pulseaudio")
    (,isc-bind "utils")))

(define %large-packages
  (list libreoffice
        python-pyqt-without-qtwebkit))

(define %spelling-packages
  (list aspell aspell-dict-en aspell-dict-ru))

(packages->manifest
 (append (list feh         ; Image viewer
               ffmpeg      ; Video, audio, images, gif conversion
               imagemagick ; Pipe to `display'

               ;; See <https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424>.
               at-spi2-core
               ghostscript/x
               ghc-pandoc  ; Convert Markdown
               gnuplot
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

               python
               python-hy

               cuirass

               go

               guile-2.2
               guile-colorized
               guile-daemon
               guile-fibers
               guile-gcrypt
               guile-git
               guile-readline
               guile-ssh
               guile-xosd

               mcron

               colordiff
               colormake
               perl

               git       ; Version control

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
               python-clf ; Interface to <https://commandlinefu.com/>
               neofetch

               xmessage

               alsa-utils
               cli-visualizer

               icecat ; Web browser

               node ;Packages in <~/.npm-global/bin/>.

               torsocks
               tor

               mailutils
               isync   ; Sync IMAP
               msmtp
               notmuch ; Mail indexer based on Xapian

               mlt         ; Video editing framework
               mpv         ; Video and audio player
               obs ;ffmpeg frontend
               vlc

               zathura     ; Lightweight customizable PDF reader
               zathura-djvu
               zathura-pdf-mupdf

               keynav
               rofi
               scrot ;screenshot
               st
               xauth
               xev
               xsel ;clipboard

               perl-uri-escape ;convert url

               patchelf ;patch elf

               octave

               hdparm
               htop            ; Pretty `top'
               inxi
               iotop
               jnettop
               python-glances
               multitail

               synergy

               ansible

               ;; FAIL: ansible         ; Configuration management
               bc
               cpio
               curl
               detox           ; Replace spaces with underscores in filenames
               diffoscope
               dos2unix
               freerdp
               graphviz  ; `dot'
               html-xml-utils
               licensecheck ; Licence checker for source files
               lsof
               ncdu            ; TUI `du'
               netcat          ; TCP
               nmap
               openssl
               parallel
               reptyr
               shellcheck
               sipcalc
               socat
               sshpass
               sqlite
               unzip
               wget
               wireshark
               zip

               texinfo

               lvm2
               cdrkit-libre
               qemu
               samba
               ubridge
               virt-manager

               python-pyqt-without-qtwebkit

               znc

               gnu-c-manual ; C language documentation
               gnu-standards
               man-pages
               sicp         ; Structure and Interpretation of Computer Programs

               chicken ; Chicken Scheme
               sbcl ; For StumpWM.  See <https://stumpwm.github.io/>.
               stumpwm

               tmux
               tmuxifier

               python-pygments ; Colorize output
               w3m

               restic ; Incremental backup

               dbus

               cflow         ;C program call map.
               gcc-toolchain ;For Emacs `semantic-mode'.
               gdb           ;GNU debuger.
               global        ;Source tagging.
               valgrind      ;Memory debug.

               gource

               mercurial

               mesa-utils
               mesa

               gst-plugins-base
               gst-plugins-bad
               gst-plugins-good
               gst-plugins-ugly
               gstreamer

               minetest                 ; Open source Minecraft

               m4

               r)

         ;; %large-packages
         %spelling-packages))
