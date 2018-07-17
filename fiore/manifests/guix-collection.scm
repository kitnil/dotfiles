(define-module (fiore manifests guix-collection)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages code)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages conkeror)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dictionaries)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnu-doc)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages kodi)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages license)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages lxde)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages node)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:export (guix-collection-packages-multiout
            guix-collection-packages
            guix-collection-packages-emacs
            guix-collection-packages-large
            guix-collection-packages-spelling
            guix-collection-packages-important))

(define guix-collection-packages-emacs
  (list

   bbdb
   emacs-academic-phrases
   emacs-ace-window
   emacs-add-hooks
   emacs-ag
   emacs-aggressive-indent  ; Auto indent minor mode
   emacs-auto-yasnippet
   emacs-bash-completion
   emacs-beginend
   emacs-browse-at-remote
   emacs-cmake-mode
   emacs-crux
   emacs-default-encrypt    ; Sign mail automatically
   emacs-default-text-scale ; Scale text in all buffers
   emacs-dumb-jump
   emacs-edit-server ; See <https://github.com/stsquad/emacs_chrome/>.
   emacs-esup
   emacs-ewmctrl            ; Control X windows from Emacs
   emacs-expand-region
   emacs-fancy-narrow
   emacs-ffap-rfc-space
   emacs-fill-column-indicator
   emacs-git-auto-commit-mode
   emacs-git-gutter
   emacs-git-timemachine
   emacs-grep-context
   emacs-guix
   emacs-helm               ; Narrowing framework
   emacs-helm-eww
   emacs-helm-firefox       ; Search for bookmarks in Icecat
   emacs-helm-gtags
   emacs-helm-make          ; Front end to `make'
   emacs-helm-mode-manager
   emacs-helm-projectile    ; Helm interface for Projectile
   emacs-helm-shell-history
   emacs-highlight-sexp
   emacs-highlight-stages   ; Highlight code stages
   emacs-highlight-symbol
   emacs-hl-todo
   emacs-hydra
   emacs-hy-mode
   emacs-ibuffer-projectile
   emacs-irfc
   emacs-ivy                ; Complition framework
   emacs-know-your-http-well
   emacs-lice-el
   emacs-macrostep
   emacs-magit-org-todos-el
   emacs-markdown-mode      ; Commonmark major mode
   emacs-mbsync
   emacs-move-text
   emacs-multiple-cursors   ; Multi cursor
   emacs-nix-mode           ; Nix language mode
   emacs-org                ; Org
   emacs-org-bullets
   emacs-org-mind-map       ; General mind maps from Org files
   emacs-outshine           ; Emacs outline-mode
   emacs-php-mode
   emacs-projectile         ; Project functions
   emacs-rainbow-delimiters ; Prettify parentheses
   emacs-rainbow-mode       ; Show colors in codes
   emacs-slime              ; Sbcl repl
   emacs-smart-mode-line
   emacs-smartparens        ; Structured editing
   emacs-suggest
   emacs-transpose-frame    ; M-x transpose-frame
   emacs-undo-tree          ; Undo visualisation
   emacs-use-package        ; Lazy configuration
   emacs-validate
   emacs-which-key          ; Key bindings help
   emacs-yaml-mode          ; YAML files
   emacs-yasnippet          ; Snippets
   emacs-yasnippet-snippets ; Collection of snippets

   magit                    ; Emacs interface for Git

   ))

(define guix-collection-packages-multiout
  (list

   (list glib "bin")
   (list git "gui")
   (list git "send-email")
   (list git "svn")
   (list alsa-plugins "pulseaudio")
   (list isc-bind "utils")
   ))

(define guix-collection-packages-large
  (list

   libreoffice ; Office suite

   ))

(define guix-collection-packages-spelling
  (list

   aspell
   aspell-dict-en
   aspell-dict-ru

   ))

(define guix-collection-packages-important
  (list

    feh         ; Image viewer
    ffmpeg      ; Video, audio, images, gif conversion
    imagemagick ; Pipe to `display'

    ;; See <https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424>.
    at-spi2-core
    ghostscript/x
    ghc-pandoc  ; Convert Markdown

    epipe
    cloc            ; Count code
    gnu-make        ; GNU Make
    recutils  ; Filter records like in `guix --search'
    stow            ; Dotfiles management
    the-silver-searcher
    woof

    translate-shell ; Translation in CLI and Emacs
    password-store  ; Password management

    python-hy

    guile-2.2
    guile-colorized
    guile-daemon
    guile-readline
    guile-xosd
    mcron

    colordiff
    colormake
    perl

    git       ; Version control

   ))

(define guix-collection-packages
  (append
   (list

    guile-commonmark ; Commonmark for Guile
    gwl              ; Guix workflow management
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

    lxappearance

    alsa-utils
    cli-visualizer

    conkeror
    icecat ; Web browser

    node ;Packages in <~/.npm-global/bin/>.

    torsocks
    tor

    isync   ; Sync IMAP
    notmuch ; Mail indexer based on Xapian

    inkscape    ; Vector graphics
    mlt         ; Video editing framework
    mpv         ; Video and audio player
    obs ;ffmpeg frontend

    zathura     ; Lightweight customizable PDF reader
    zathura-djvu
    zathura-pdf-mupdf

    keynav
    rofi
    scrot ;screenshot
    xsel ;clipboard
    st

    perl-uri-escape ;convert url

    patchelf ;patch elf

    octave

    python-glances
    htop            ; Pretty `top'
    inxi

    ;; FAIL: ansible         ; Configuration management
    bc
    cpio
    detox           ; Replace spaces with underscores in filenames
    diffoscope
    dos2unix
    freerdp
    graphviz  ; `dot'
    licensecheck ; Licence checker for source files
    lsof
    html-xml-utils
    ncdu            ; TUI `du'
    netcat          ; TCP
    nmap
    openssl
    parallel
    reptyr
    shellcheck
    socat
    sqlite
    unzip
    zip

    texinfo

    virt-manager
    qemu
    samba

    emacs-znc
    znc

    gnu-c-manual ; C language documentation
    gnu-standards
    man-pages
    sicp         ; Structure and Interpretation of Computer Programs
    texlive

    chicken ; Chicken Scheme
    sbcl ; For StumpWM.  See <https://stumpwm.github.io/>.
    sbcl-stumpwm

    python-pygments ; Colorize output
    w3m

    restic ; Incremental backup

    dbus

    cflow         ;C program call map.
    gcc-toolchain ;For Emacs `semantic-mode'.
    gdb           ;GNU debuger.
    global        ;Source tagging.
    valgrind      ;Memory debug.

    emacs-company            ; Complition framework
    emacs-company-quickhelp  ; Help pages for Company
    emacs-constants
    emacs-debbugs ; <https://debbugs.gnu.org/> interface
    emacs-elfeed             ; RSS reader
    emacs-erc-hl-nicks       ; for ERC
    emacs-f3
    emacs-ggtags             ; Front end to GNU Global
    emacs-gitpatch           ; Send patches
    emacs-god-mode           ; Commands without modifier keys
    emacs-htmlize
    emacs-lua-mode
    emacs-nginx-mode
    emacs-restclient
    emacs-scheme-complete
    emacs-transmission       ; Front end to transmission-daemon
    emacs-w3m                ; Front end to w3m command line web browser
    ;; haskell-mode

    flycheck                 ; Syntax checker
    geiser                   ; Scheme bridge

    gource

    mercurial

    gst-plugins-bad
    gst-plugins-base
    gst-plugins-good
    gst-plugins-ugly
    gstreamer

    minetest                 ; Open source Minecraft

    )

   guix-collection-packages-emacs
   guix-collection-packages-large
   guix-collection-packages-spelling
   guix-collection-packages-important))
