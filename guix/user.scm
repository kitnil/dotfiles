(use-package-modules admin
		     aspell
		     autotools
		     backup
		     bittorrent
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
		     llvm
		     m4
		     mail
		     ncdu
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
       conkeror
       emacs-auctex
       emacs-cider
       emacs-company
       emacs-debbugs
       emacs-dired-hacks
       emacs-git-gutter
       emacs-git-timemachine
       emacs-ob-ipython
       emacs-org
       emacs-pdf-tools
       emacs-projectile
       emacs-rainbow-delimiters
       emacs-rainbow-identifiers
       emacs-sx
       emacs-transpose-frame
       emacs-undo-tree
       emacs-use-package
       emacs-which-key
       emacs-writegood-mode
       emacs-yasnippet
       feh
       gcc
       gdb
       git
       gnu-c-manual
       gnu-gettext
       gnu-make
       gnupg
       graphviz
       gs-fonts
       guile-2.2
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
       mercurial
       mpv
       ncdu
       neofetch
       nmap
       notmuch
       offlineimap
       openssh
       parallel
       paredit
       password-store
       pavucontrol
       perl
       pinentry
       pkg-config
       ponymix
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
       xdotool
       xev
       xmodmap
       xprop
       xrdb
       youtube-dl))
