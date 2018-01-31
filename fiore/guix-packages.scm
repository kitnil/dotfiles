(define-module (guix-packages)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:export (guix-collection-packages))

(use-package-modules admin aspell audio bittorrent code conkeror
commencement compression databases dictionaries emacs games gcc glib
gnuzilla graphics gtk guile haskell image-viewers imagemagick kodi
libreoffice linux lisp mail man messaging ncdu ncurses
package-management password-utils pdf python rdesktop samba scheme tls
valgrind version-control video virtualization w3m web xdisorg)

(define guix-collection-packages-multi
  (list
   (list glib "bin")))

(define guix-collection-packages-big
   obs         ; OBS Studio
   )

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
   youtube-dl   ; Video and music from websites

   redshift  ; Color temperature
   python-clf ; Interface to <https://commandlinefu.com/>
   neofetch

   lxappearance

   cli-visualizer

   conkeror
   icecat ; Web browser

   isync   ; Sync IMAP
   notmuch ; Mail indexer based on Xapian

   blender
   feh         ; Image viewer
   ffmpeg      ; Video, audio, images, gif conversion
   mlt         ; Video editing framework
   imagemagick ; Pipe to `display'
   mpv         ; Video and audio player

   ;; See <https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424>.
   at-spi2-core

   libreoffice ; Office suite
   mupdf       ; Lightweight PDF reader
   ghc-pandoc  ; Convert Markdown

   cloc            ; Count code
   diffoscope
   freerdp
   gnu-make        ; GNU Make
   grep
   htop            ; Pretty `top'
   lm-sensors      ; `sensors'
   ncdu            ; TUI `du'
   ncurses
   netcat          ; TCP
   nmap
   openssl
   shellcheck
   sqlite
   stow            ; Dotfiles management
   the-silver-searcher
   unzip

   qemu
   samba

   translate-shell ; Translation in CLI and Emacs

   emacs-znc
   znc

   gnu-c-manual ; C language documentation
   man-pages
   sicp         ; Structure and Interpretation of Computer Programs

   chicken ; Chicken Scheme
   sbcl ; For StumpWM.  See <https://stumpwm.github.io/>.

   python-pygments ; Colorize output
   w3m

   password-store  ; Password management

   dbus

   gcc-toolchain ; For Emacs `semantic-mode'
   cflow         ; C program call map
   global        ; Source tagging
   valgrind
   emacs-ag
   emacs-aggressive-indent  ; Auto indent minor mode
   emacs-company            ; Complition framework
   emacs-company-quickhelp  ; Help pages for Company
   emacs-constants
   emacs-default-encrypt    ; Sign mail automatically
   emacs-elfeed             ; RSS reader
   emacs-emms-player-mpv    ; Frontend to MPV for Emms
   emacs-erc-hl-nicks       ; for ERC
   emacs-expand-region
   emacs-ffap-rfc-space
   emacs-git-messenger      ; Show commit under the cursor
   emacs-ggtags             ; Front end to GNU Global
   emacs-git-gutter
   emacs-gitpatch           ; Send patches
   emacs-god-mode           ; Commands without modifier keys
   emacs-helm               ; Narrowing framework
   emacs-helm-make          ; Front end to `make'
   emacs-helm-projectile    ; Helm interface for Projectile
   emacs-highlight-stages   ; Highlight code stages
   emacs-highlight-symbol
   emacs-htmlize
   emacs-ivy                ; Complition framework
   emacs-lua-mode
   emacs-markdown-mode      ; Commonmark major mode
   emacs-multiple-cursors   ; Multi cursor
   emacs-nix-mode           ; Nix language mode
   emacs-org                ; Org
   emacs-projectile         ; Project functions
   emacs-rainbow-delimiters ; Prettify parentheses
   emacs-rainbow-mode       ; Show colors in codes
   emacs-slime              ; Sbcl repl
   emacs-smartparens        ; Structured editing
   emacs-transmission       ; Front end to transmission-daemon
   emacs-transpose-frame    ; M-x transpose-frame
   emacs-undo-tree          ; Undo visualisation
   emacs-use-package        ; Lazy configuration
   emacs-w3m                ; Front end to w3m command line web browser
   emacs-which-key          ; Key bindings help
   emacs-yasnippet          ; Snippets
   emacs-yasnippet-snippets ; Collection of snippets
   emms                     ; Video and music player
   haskell-mode

   flycheck                 ; Syntax checker
   geiser                   ; Scheme bridge
   git
   magit                    ; Emacs interface for Git
   mercurial

   guile-daemon
   mcron

   minetest                 ; Open source Minecraft
   tome4                    ; Tails of Maj'Eyal
   ))
