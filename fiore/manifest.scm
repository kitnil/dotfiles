(use-modules (gnu)
             (guix packages)
             (wigust packages emacs)
             (wigust packages licensecheck)
             (wigust packages pulseaudio)
             (wigust packages python)
             (wigust packages version-control))

(use-package-modules admin aspell audio bittorrent code conkeror
commencement compression cran databases dictionaries emacs games gcc
glib gnuzilla graphics gtk guile haskell image-viewers imagemagick
kodi libreoffice linux lisp lxde mail man maths messaging ncdu ncurses
package-management password-utils patchutils perl pdf python rdesktop
samba scheme statistics texinfo textutils tls valgrind version-control
video virtualization w3m web xdisorg)

(define guix-collection-packages-multiout
  (list
   (list glib "bin")))

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

   streamlink
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
   imagemagick ; Pipe to `display'
   mlt         ; Video editing framework
   mpv         ; Video and audio player

   ;; See <https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424>.
   at-spi2-core

   ghc-pandoc  ; Convert Markdown
   libreoffice ; Office suite
   zathura     ; Lightweight customizable PDF reader
   zathura-pdf-mupdf

   epipe

   cloc            ; Count code
   diffoscope
   dos2unix
   freerdp
   gnu-make        ; GNU Make
   graphviz  ; `dot'
   grep
   htop            ; Pretty `top'
   ncdu            ; TUI `du'
   ncurses
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
   man-pages
   sicp         ; Structure and Interpretation of Computer Programs

   chicken ; Chicken Scheme
   sbcl ; For StumpWM.  See <https://stumpwm.github.io/>.

   python-pygments ; Colorize output
   w3m

   duplicity ; Incremental backup
   password-store  ; Password management

   dbus

   cflow         ; C program call map
   gcc-toolchain ; For Emacs `semantic-mode'
   global        ; Source tagging
   valgrind

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
   emacs-emms-player-mpv    ; Frontend to MPV for Emms
   emacs-erc-hl-nicks       ; for ERC
   emacs-expand-region
   emacs-ffap-rfc-space
   emacs-ggtags             ; Front end to GNU Global
   emacs-git-gutter
   emacs-git-messenger      ; Show commit under the cursor
   emacs-gitpatch           ; Send patches
   emacs-god-mode           ; Commands without modifier keys
   emacs-helm               ; Narrowing framework
   emacs-helm-make          ; Front end to `make'
   emacs-helm-projectile    ; Helm interface for Projectile
   emacs-highlight-sexp
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
   gource
   magit                    ; Emacs interface for Git
   mercurial

   guile-daemon
   guile-colorized
   mcron

   minetest                 ; Open source Minecraft
   tome4                    ; Tails of Maj'Eyal

   colordiff
   colormake
   perl

   octave))

(define guix-wigust-packages
  (list

   pulsemixer-emacs-keybindings
   emacs-athena
   emacs-beginend
   emacs-engine-mode-autoload ; Define searches on websites
   emacs-strace-mode-special ; Colorize `strace' logs

   emacs-edit-server ; See <https://github.com/stsquad/emacs_chrome/>.

   python-starred ; Fetch a list of stars from GitHub user

   emacs-add-hooks
   emacs-crux
   emacs-debpaste           ; Front end to <https://paste.debian.net/>
   emacs-default-text-scale ; Scale text in all buffers
   emacs-dumb-jump
   emacs-esup
   emacs-eval-in-repl       ; Evaluate to different Repls
   emacs-ewmctrl            ; Control X windows from Emacs
   emacs-fancy-narrow
   emacs-guix-checkout      ; Guix interface
   emacs-helm-c-yasnippet
   emacs-helm-emms
   emacs-helm-firefox       ; Search for bookmarks in Icecat
   emacs-helm-gtags
   emacs-helm-mode-manager
   emacs-helm-pass          ; Front end to password-store
   emacs-ibuffer-projectile
   emacs-info-colors
   emacs-info-colors        ; Colorize info pages
   emacs-move-text
   emacs-org-mind-map       ; General mind maps from Org files
   emacs-scratch-el
   emacs-terminal-here

   licensecheck ; Licence checker for source files

   vc-dwim-git-worktree))

(packages->manifest `(,@guix-collection-packages
                      ,@guix-collection-packages-multiout
                      ,@guix-wigust-packages))
