(use-modules (gnu) (guix packages)
             (wigust packages emacs) (wigust packages python))

(use-package-modules admin aspell audio bittorrent code conkeror
commencement compression databases dictionaries emacs games gcc glib
gnuzilla guile haskell image-viewers imagemagick kodi libreoffice
linux lisp mail man messaging ncdu ncurses package-management
password-utils pdf python scheme tls version-control video w3m web
xdisorg)

(packages->manifest
 (list
  aspell
  aspell-dict-en
  aspell-dict-ru

  guile-commonmark ; Commonmark for Guile
  ;; gwl              ; Guix workflow management
  haunt            ; Guile static site generator

  aria2        ; Download utility
  kodi-cli     ; Remote control Kodi
  transmission ; Bittorrent
  youtube-dl   ; Video and music from websites

  redshift  ; Color temperature
  python-clf ; Interface to <https://commandlinefu.com/>
  neofetch

  gource ; 3D visualisation tool for source control repositories

  cli-visualizer

  conkeror
  icecat ; Web browser

  isync   ; Sync IMAP
  notmuch ; Mail indexer based on Xapian

  feh         ; Image viewer
  ffmpeg      ; Video, audio, images, gif conversion
  imagemagick ; Pipe to `display'
  mpv         ; Video and audio player
  obs         ; OBS Studio

  libreoffice ; Office suite
  xpdf        ; Standart PDF reader XAW
  ghc-pandoc  ; Convert Markdown

  cloc            ; Count code
  diffoscope
  gnu-make        ; GNU Make
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

  translate-shell ; Translation in CLI and Emacs

  emacs-znc
  znc

  gnu-c-manual ; C language documentation
  man-pages
  sicp         ; Structure and Interpretation of Computer Programs

  sbcl ; For StumpWM.  See <https://stumpwm.github.io/>.

  python-pygments ; Colorize output
  w3m

  password-store  ; Password management

  (list glib "bin")
  gcc-toolchain ; For Emacs `semantic-mode'
  cflow         ; C program call map
  global        ; Source tagging

  emacs-athena
  emacs-engine-mode-autoload ; Define searches on websites
  emacs-strace-mode-special ; Colorize `strace' logs

  python-starred ; Fetch a list of stars from GitHub user

  emacs-eval-in-repl       ; Evaluate to different Repls
  emacs-add-hooks
  emacs-crux
  emacs-debpaste           ; Front end to <https://paste.debian.net/>
  emacs-esup
  emacs-ewmctrl            ; Control X windows from Emacs
  emacs-fancy-narrow
  emacs-helm-firefox       ; Search for bookmarks in Icecat
  emacs-helm-gtags
  emacs-helm-mode-manager
  emacs-helm-pass          ; Front end to password-store
  emacs-move-text
  emacs-org-mind-map       ; General mind maps from Org files

  emacs-aggressive-indent  ; Auto indent minor mode
  emacs-beginend
  emacs-company            ; Complition framework
  emacs-company-quickhelp  ; Help pages for Company
  emacs-debbugs-with-bugs  ; <https://debbugs.gnu.org/> interface
  emacs-default-encrypt    ; Sign mail automatically
  emacs-default-text-scale ; Scale text in all buffers
  emacs-elfeed             ; RSS reader
  emms                     ; Video and music player
  emacs-emms-player-mpv    ; Frontend to MPV for Emms
  emacs-erc-hl-nicks       ; for ERC
  emacs-expand-region
  emacs-ffap-rfc-space
  emacs-ggtags             ; Front end to GNU Global
  emacs-git-gutter
  emacs-gitpatch           ; Send patches
  emacs-god-mode           ; Commands without modifier keys
  emacs-guix-checkout      ; Guix interface
  emacs-helm               ; Narrowing framework
  emacs-helm-emms
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
  haskell-mode

  flycheck                 ; Syntax checker
  geiser                   ; Scheme bridge
  magit                    ; Emacs interface for Git

  tome4                    ; Tails of Maj'Eyal
  ))
