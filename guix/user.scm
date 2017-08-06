(use-modules (gnu)
             (ng0 packages))

(use-package-modules
 admin
 android
 aspell
 autotools
 backup
 bittorrent
 commencement
 compression
 conkeror
 databases
 dictionaries
 emacs
 file
 fonts
 fontutils
 gcc
 gdb
 gettext
 ghostscript
 gimp
 gnu-doc
 gnupg
 gnuzilla
 graphviz
 guile
 haskell
 imagemagick
 image-viewers
 java
 libreoffice
 linux
 lisp
 llvm
 m4
 mail
 man
 maths
 ncdu
 networking
 ocaml
 package-management
 parallel
 password-utils
 perl
 pkg-config
 pulseaudio
 qemu
 qt
 rsync
 samba
 scanner
 scheme
 screen
 shells
 spice
 ssh
 tex
 texinfo
 tmux
 version-control
 video
 web
 wget
 wm
 xdisorg
 xorg)



(packages->manifest
 (list
  adb
  alsa-utils
  aria2
  aspell
  aspell-dict-en
  aspell-dict-ru
  autoconf
  automake
  bzip2
  chromium
  conkeror
  emacs-ag
  emacs-aggressive-indent
  emacs-async
  emacs-auctex
  emacs-cider
  emacs-company
  emacs-debbugs
  emacs-dired-hacks
  emacs-expand-region
  emacs-git-gutter
  emacs-git-timemachine
  emacs-hydra
  emacs-lua-mode
  emacs-multiple-cursors
  emacs-org
  emacs-paredit
  emacs-pdf-tools
  emacs-projectile
  emacs-rainbow-delimiters
  emacs-rainbow-identifiers
  emacs-rainbow-mode
  emacs-rpm-spec-mode
  emacs-sx
  emacs-transpose-frame
  emacs-undo-tree
  emacs-use-package
  emacs-web-mode
  emacs-which-key
  emacs-writegood-mode
  emacs-yasnippet
  emacs-yasnippet-snippets
  ethtool
  feh
  ffmpeg
  file
  font-adobe-source-han-sans
  fontconfig
  font-hack
  font-wqy-zenhei
  gcc-toolchain
  gdb
  gettext
  ghc
  ghc-xmonad-contrib
  ghostscript-with-x
  gimp
  git
  git
  gnu-c-manual
  gnupg
  gnuplot
  gs-fonts
  guile
  guile-git
  guile-ssh
  guix
  haskell-mode
  haunt
  help2man
  htop
  icecat
  icedtea
  iperf
  isync
  json-c
  libgcrypt
  libreoffice
  m4
  magit
  mailutils
  make
  maxima
  mercurial
  mps-youtube
  mpv
  mysql
  ncdu
  neofetch
  nmap
  node
  notmuch
  ocaml
  openssh
  paperkey
  parallel
  password-store
  pavucontrol
  perl
  php
  pinentry
  pkg-config
  ponymix
  proof-general
  pulseaudio
  python-ghp-import
  python-internetarchive
  qemu
  racket
  redshift
  rsnapshot
  rsync
  rxvt-unicode
  samba
  screen
  setxkbmap
  shellcheck
  sicp
  sqlite
  stow
  strace
  texinfo
  texlive
  the-silver-searcher
  tmux
  tome4
  translate-shell
  tree
  unzip
  vera
  virt-viewer
  wakelan
  wget
  wgetpaste
  xclip
  xclip
  xdotool
  xev
  xinit
  xlsfonts
  xmobar
  xmodmap
  xmonad
  xorg-server
  xprop
  xrdb
  xset
  youtube-dl))
