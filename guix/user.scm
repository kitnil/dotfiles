(use-package-modules admin
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
		     fonts
		     gcc
		     gettext
		     gdb
		     ghostscript
		     gnu-doc
		     gnupg
		     gnuzilla
		     graphviz
		     guile
		     image-viewers
		     imagemagick
		     inkscape
		     java
		     libreoffice
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
		     python
		     qemu
		     qt
		     rsync
		     scanner
		     scheme
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
		     xdisorg
		     xorg)

(packages->manifest
 (list aria2
       ;; BUG: python-scipy
       aspell
       aspell-dict-en
       aspell-dict-ru
       autoconf
       automake
       bzip2
       clang
       clojure
       conkeror
       emacs-auctex
       emacs-cider
       emacs-company
       emacs-debbugs
       emacs-dired-hacks
       emacs-expand-region
       emacs-git-gutter
       emacs-git-timemachine
       emacs-ob-ipython
       emacs-org
       emacs-pdf-tools
       emacs-projectile
       emacs-rainbow-delimiters
       emacs-rainbow-identifiers
       emacs-slime
       emacs-sx
       emacs-transpose-frame
       emacs-undo-tree
       emacs-use-package
       emacs-which-key
       emacs-writegood-mode
       emacs-yasnippet
       ethtool
       feh
       ffmpeg
       gcc-toolchain-7
       gcl
       gdb
       ghostscript/x
       git
       (list git "send-email")
       gnu-c-manual
       gnu-gettext
       gnu-make
       gnupg
       gs-fonts
       guile-2.2
       guile-ssh
       guix
       help2man
       htop
       icecat
       icedtea
       inkscape
       json-c
       jupyter
       libgcrypt
       libreoffice
       m4
       magit
       maxima
       mercurial
       mpv
       mps-youtube
       ncdu
       neofetch
       nmap
       notmuch
       offlineimap
       openssh
       paperkey
       parallel
       paredit
       password-store
       pavucontrol
       perl
       pinentry
       pkg-config
       ponymix
       proof-general
       pulseaudio
       python
       python-matplotlib
       python-pyqt
       python-sphinx
       python-sympy
       qemu
       racket
       redshift
       rsnapshot
       rsync
       rxvt-unicode
       setxkbmap
       sicp
       sqlite
       stow
       texinfo
       texlive
       tmux
       translate-shell
       tree
       vera
       virt-viewer
       wget
       xclip
       xdotool
       xev
       xmodmap
       xprop
       xrdb
       youtube-dl
       wakelan))
