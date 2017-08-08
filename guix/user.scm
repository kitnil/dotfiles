(use-modules (gnu)
             (ng0 packages chromium))

(use-package-modules
 admin backup compression databases gnupg linux networking package-management
 bittorrent rsync mail samba ssh wget
 file ncdu password-utils scanner xdisorg xorg
 screen tmux
 man shells
 spice virtualization
 android
 aspell
 autotools commencement gcc gdb gnu-doc m4 parallel pkg-config qt
 llvm
 code version-control
 emacs dictionaries
 games
 gettext
 graphviz ghostscript texinfo tex
 web conkeror gnuzilla
 image-viewers imagemagick gimp
 maths libreoffice
 guile node haskell lisp java ocaml perl php scheme
 pulseaudio video
 fonts fontutils wm)



(packages->manifest
 (list
  ;; Sound
  alsa-utils pulseaudio ponymix pavucontrol password-store

  ;; Spelling
  aspell aspell-dict-en aspell-dict-ru shellcheck

  ;; Translation
  translate-shell gnu-gettext

  ;; Compression
  bzip2 unzip

  ;; Git
  magit emacs-git-gutter emacs-git-timemachine
  git

  ;; Version control
  mercurial

  ;; Guile
  guile-2.2 guile-git guile-ssh guix

  ;; LaTex & Tex & Texinfo
  ghostscript/x gs-fonts emacs-auctex texlive texinfo

  ;; Lua
  emacs-lua-mode

  ;; Web
  node php emacs-web-mode
  conkeror icecat chromium
  python-internetarchive

  ;; Static site
  haunt python-ghp-import

  ;; Clojure & Java
  icedtea emacs-cider

  ;; RPM
  emacs-rpm-spec-mode

  ;; Common Lisp
  emacs-sx

  ;; Racket
  racket

  ;; Haskell
  haskell-mode ghc xmobar xmonad ghc-xmonad-contrib

  ;; Ocaml
  ocaml

  ;; org-mode
  emacs-org emacs-org-edit-latex emacs-org-pomodoro

  ;; Text search
  emacs-ag
  the-silver-searcher

  ;; Emacs
  emacs-company emacs-undo-tree emacs-projectile
  emacs-multiple-cursors paredit emacs-aggressive-indent emacs-transpose-frame
  emacs-expand-region
  emacs-dired-hacks emacs-pdf-tools
  emacs-which-key
  emacs-yasnippet emacs-yasnippet-snippets
  emacs-writegood-mode
  emacs-rainbow-delimiters emacs-rainbow-identifiers emacs-rainbow-mode
  emacs-async emacs-use-package

  ;; Images
  feh gimp

  ;; Videos
  ffmpeg mpv mps-youtube youtube-dl

  ;; Admin
  file htop ncdu neofetch tree stow strace parallel adb

  ;; Fonts
  fontconfig font-adobe-source-han-sans font-hack font-wqy-zenhei

  ;; C language
  gcc-toolchain-7 gdb gnu-c-manual json-c
  autoconf automake  gnu-make m4 pkg-config
  clang help2man

  ;; Gnupg
  gnupg libgcrypt pinentry paperkey

  ;; Office
  libreoffice

  ;; Mail
  isync mailutils notmuch emacs-debbugs

  ;; Math
  maxima proof-general gnuplot

  ;; Databases
  mysql sqlite

  ;; Networking
  openssh nmap ethtool wakelan iperf

  ;; Perl
  perl

  ;; Virtualization
  qemu virt-viewer

  ;; Rsync
  rsync rsnapshot

  ;; SMB
  samba

  ;; Terminal application
  rxvt-unicode

  ;; Terminal multiplexer
  screen tmux

  ;; Documentation & Books
  vera sicp

  ;; Games
  tome4

  ;; Downloading
  aria2 wget

  ;; Clipboard
  xclip wgetpaste

  ;; Xorg
  xdotool xev xinit xlsfonts xorg-server xprop xrdb xset xmodmap setxkbmap
  redshift))
