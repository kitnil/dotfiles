(define-module (fiore manifests guix-collection)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:export (guix-collection-packages-multiout
            guix-collection-packages))

(use-package-modules admin aspell audio backup bittorrent code
conkeror commencement compression cran databases elf dictionaries
emacs games gcc gdb glib gnu-doc gnuzilla graphics graphviz gstreamer
gtk guile haskell image-viewers imagemagick kodi libreoffice linux
lisp lxde mail man maths messaging ncdu ncurses node
package-management password-utils patchutils perl perl-web pdf python
rdesktop samba scheme statistics texinfo textutils tls tor valgrind
version-control video virtualization w3m web xdisorg)

(define guix-collection-packages-multiout
  (list
   (list glib "bin")
   (list git "gui")
   (list git "send-email")
   (list git "svn")))

(define guix-collection-packages
  (list

   aspell
   aspell-dict-en
   aspell-dict-ru

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

   cli-visualizer

   conkeror
   icecat ; Web browser

   node ;Packages in <~/.npm-global/bin/>.

   torsocks
   tor

   isync   ; Sync IMAP
   notmuch ; Mail indexer based on Xapian

   blender
   feh         ; Image viewer
   ffmpeg      ; Video, audio, images, gif conversion
   imagemagick ; Pipe to `display'
   mlt         ; Video editing framework
   mpv         ; Video and audio player
   obs ;ffmpeg frontend

   ;; See <https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424>.
   at-spi2-core

   ghc-pandoc  ; Convert Markdown
   libreoffice ; Office suite
   zathura     ; Lightweight customizable PDF reader
   zathura-pdf-mupdf

   xsel ;clipboard
   scrot ;screenshot

   perl-uri-escape ;convert url

   patchelf ;patch elf

   epipe

   cloc            ; Count code
   detox           ; Replace spaces with underscores in filenames
   diffoscope
   dos2unix
   freerdp
   gnu-make        ; GNU Make
   graphviz  ; `dot'
   htop            ; Pretty `top'
   ncdu            ; TUI `du'
   netcat          ; TCP
   nmap
   openssl
   recutils  ; Filter records like in `guix --search'
   shellcheck
   sqlite
   stow            ; Dotfiles management
   the-silver-searcher
   unzip
   zip

   texinfo

   qemu
   samba

   translate-shell ; Translation in CLI and Emacs

   emacs-znc
   znc

   gnu-c-manual ; C language documentation
   gnu-standards
   man-pages
   sicp         ; Structure and Interpretation of Computer Programs

   chicken ; Chicken Scheme
   sbcl ; For StumpWM.  See <https://stumpwm.github.io/>.

   python-pygments ; Colorize output
   w3m

   duplicity ; Incremental backup
   password-store  ; Password management

   dbus

   cflow         ;C program call map.
   gcc-toolchain ;For Emacs `semantic-mode'.
   gdb           ;GNU debuger.
   global        ;Source tagging.
   valgrind      ;Memory debug.

   bbdb
   emacs-ag
   emacs-aggressive-indent  ; Auto indent minor mode
   emacs-browse-at-remote
   emacs-company            ; Complition framework
   emacs-company-quickhelp  ; Help pages for Company
   emacs-constants
   emacs-debbugs ; <https://debbugs.gnu.org/> interface
   emacs-default-encrypt    ; Sign mail automatically
   emacs-elfeed             ; RSS reader
   emacs-erc-hl-nicks       ; for ERC
   emacs-expand-region
   emacs-ffap-rfc-space
   emacs-fill-column-indicator
   emacs-ggtags             ; Front end to GNU Global
   emacs-git-gutter
   emacs-git-timemachine
   emacs-gitpatch           ; Send patches
   emacs-god-mode           ; Commands without modifier keys
   emacs-helm               ; Narrowing framework
   emacs-helm-make          ; Front end to `make'
   emacs-helm-projectile    ; Helm interface for Projectile
   emacs-highlight-sexp
   emacs-highlight-stages   ; Highlight code stages
   emacs-highlight-symbol
   emacs-hl-todo
   emacs-htmlize
   emacs-hydra
   emacs-ivy                ; Complition framework
   emacs-lua-mode
   emacs-markdown-mode      ; Commonmark major mode
   emacs-multiple-cursors   ; Multi cursor
   emacs-nix-mode           ; Nix language mode
   emacs-org                ; Org
   emacs-org-bullets
   emacs-php-mode
   emacs-projectile         ; Project functions
   emacs-rainbow-delimiters ; Prettify parentheses
   emacs-rainbow-mode       ; Show colors in codes
   emacs-restclient
   emacs-slime              ; Sbcl repl
   emacs-smart-mode-line
   emacs-smartparens        ; Structured editing
   emacs-transmission       ; Front end to transmission-daemon
   emacs-transpose-frame    ; M-x transpose-frame
   emacs-undo-tree          ; Undo visualisation
   emacs-use-package        ; Lazy configuration
   emacs-w3m                ; Front end to w3m command line web browser
   emacs-which-key          ; Key bindings help
   emacs-yaml-mode          ; YAML files
   emacs-yasnippet          ; Snippets
   emacs-yasnippet-snippets ; Collection of snippets
   emms                     ; Video and music player
   haskell-mode

   flycheck                 ; Syntax checker
   geiser                   ; Scheme bridge

   git       ; Version control
   gource
   magit                    ; Emacs interface for Git

   mercurial

   gst-plugins-bad
   gst-plugins-base
   gst-plugins-good
   gst-plugins-ugly
   gstreamer

   guile-2.2
   guile-readline
   guile-daemon
   guile-colorized
   mcron

   minetest                 ; Open source Minecraft

   colordiff
   colormake
   perl

   octave))
