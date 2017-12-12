(use-modules (gnu) (guix packages) (wigust packages emacs))

(use-package-modules admin aspell bittorrent code commencement
compression dictionaries emacs gcc glib gnuzilla guile haskell
image-viewers imagemagick kodi libreoffice linux lisp mail messaging
ncdu package-management password-utils python scheme tls video w3m web
xdisorg)

(packages->manifest
 (list
  aspell
  aspell-dict-en
  aspell-dict-ru

  guile-commonmark ; Commonmark for Guile
  gwl              ; Guix workflow management
  haunt            ; Guile static site generator

  kodi-cli     ; Remote control Kodi
  transmission ; Bittorrent
  youtube-dl   ; Video and music from websites

  redshift  ; Color temperature
  python-clf ; Interface to <https://commandlinefu.com/>

  icecat ; Web browser

  isync   ; Sync IMAP
  notmuch ; Mail indexer based on Xapian

  feh         ; Image viewer
  ffmpeg      ; Video, audio, images, gif conversion
  imagemagick ; Pipe to `display'
  mpv         ; Video and audio player
  obs         ; OBS Studio

  libreoffice ; Office suite
  ghc-pandoc  ; Convert Markdown

  htop            ; Pretty `top'
  unzip
  shellcheck
  stow            ; Dotfiles management
  gnu-make        ; GNU Make
  cloc            ; Count code
  lm-sensors      ; `sensors'
  ncdu            ; TUI `du'
  netcat          ; TCP
  nmap
  openssl

  translate-shell ; Translation in CLI and Emacs

  emacs-znc
  znc

  gnu-c-manual ; C language documentation
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

  emacs-debpaste           ; Front end to <https://paste.debian.net/>
  emacs-helm-firefox       ; Search for bookmarks in Icecat
  emacs-helm-pass          ; Front end to password-store
  emacs-eval-in-repl       ; Evaluate to different Repls
  emacs-ewmctrl            ; Control X windows from Emacs
  emacs-org-mind-map       ; General mind maps from Org files
  emacs-move-text
  emacs-strace-mode-special ; Colorize `strace' logs

  emacs-aggressive-indent  ; Auto indent minor mode
  emacs-company            ; Complition framework
  emacs-company-quickhelp  ; Help pages for Company
  emacs-debbugs            ; <https://debbugs.gnu.org/> interface
  emacs-elfeed             ; RSS reader
  emacs-erc-hl-nicks       ; for ERC
  emacs-ggtags             ; Front end to GNU Global
  emacs-git-gutter
  emacs-gitpatch           ; Send patches
  emacs-guix               ; Guix interface
  emacs-helm               ; Narrowing framework
  emacs-helm-make          ; Front end to `make'
  emacs-helm-projectile    ; Helm interface for Projectile
  emacs-highlight-stages   ; Highlight code stages
  emacs-ivy                ; Complition framework
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
  emacs-use-package        ; Lazy configuration
  emacs-w3m                ; Front end to w3m command line web browser
  emacs-which-key          ; Key bindings help
  emacs-yasnippet          ; Snippets
  emacs-yasnippet-snippets ; Collection of snippets
  flycheck                 ; Syntax checker
  geiser                   ; Scheme bridge
  magit                    ; Emacs interface for Git
  ))
