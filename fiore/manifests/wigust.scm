(define-module (fiore manifests wigust)
  #:use-module (wigust packages emacs)
  #:use-module (wigust packages licensecheck)
  #:use-module (wigust packages pulseaudio)
  #:use-module (wigust packages python)
  #:use-module (wigust packages version-control)
  #:use-module (local packages emacs)
  #:use-module (gnu packages emacs)
  #:export (guix-wigust-packages))

(define guix-wigust-packages
  (list

   pulsemixer-emacs-keybindings
   emacs-athena
   emacs-beginend
   emacs-engine-mode-autoload ; Define searches on websites
   emacs-strace-mode-special ; Colorize `strace' logs

   emacs-edit-server ; See <https://github.com/stsquad/emacs_chrome/>.

   python-starred ; Fetch a list of stars from GitHub user

   emacs-academic-phrases
   emacs-add-hooks
   emacs-anywhere-mode
   emacs-atomic-chrome
   emacs-auto-yasnippet
   emacs-awk-it
   emacs-crux
   emacs-debpaste           ; Front end to <https://paste.debian.net/>
   emacs-default-text-scale ; Scale text in all buffers
   emacs-dumb-jump
   emacs-edit-indirect
   emacs-emamux
   emacs-emms-player-simple-mpv ; Frontend to MPV for Emms
   emacs-epkg
   emacs-esup
   emacs-eval-in-repl       ; Evaluate to different Repls
   emacs-ewmctrl            ; Control X windows from Emacs
   emacs-f3
   emacs-fancy-narrow
   emacs-git-auto-commit-mode
   emacs-git-messenger-diff-mode
   emacs-grep-context
   emacs-guix-local-checkout ; Guix interface
   emacs-helm-c-yasnippet
   emacs-helm-emms
   emacs-helm-firefox       ; Search for bookmarks in Icecat
   emacs-helm-gtags
   emacs-helm-mode-manager
   emacs-helm-navi
   emacs-helm-pass          ; Front end to password-store
   emacs-helm-shell-history
   emacs-ibuffer-projectile
   emacs-info-colors
   emacs-info-colors        ; Colorize info pages
   emacs-irfc
   emacs-know-your-http-well
   emacs-lice-el
   emacs-macrostep
   emacs-magit-org-todos-el
   emacs-mbsync
   emacs-move-text
   emacs-org-mind-map       ; General mind maps from Org files
   emacs-outshine           ; Emacs outline-mode
   emacs-pcre2el
   emacs-redshift
   emacs-scratch-el
   emacs-suggest
   emacs-terminal-here-checkout
   emacs-validate

   licensecheck ; Licence checker for source files

   vc-dwim-git-worktree))
