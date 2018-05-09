(define-module (fiore manifests wigust)
  #:use-module (wigust packages emacs)
  #:use-module (wigust packages pulseaudio)
  #:use-module (wigust packages python)
  #:use-module (wigust packages version-control)
  #:use-module (local packages emacs)
  #:use-module (local packages xdisorg)
  #:use-module (gnu packages emacs)
  #:export (guix-wigust-packages
            guix-wigust-packages-emacs))

(define guix-wigust-packages-emacs
  (list
   emacs-athena
   emacs-engine-mode-autoload ; Define searches on websites
   emacs-strace-mode-special ; Colorize `strace' logs
   emacs-add-hooks
   emacs-anywhere-mode
   emacs-auto-yasnippet
   emacs-crux
   emacs-default-text-scale ; Scale text in all buffers
   emacs-dumb-jump
   emacs-emamux
   emacs-eval-in-repl       ; Evaluate to different Repls
   emacs-ewmctrl            ; Control X windows from Emacs
   emacs-git-auto-commit-mode
   emacs-git-messenger-diff-mode
   emacs-grep-context
   emacs-helm-c-yasnippet
   emacs-helm-eww
   emacs-helm-firefox       ; Search for bookmarks in Icecat
   emacs-helm-mode-manager
   emacs-helm-shell-history
   emacs-helm-pass          ; Front end to password-store
   emacs-ibuffer-projectile
   emacs-info-colors        ; Colorize info pages
   emacs-irfc
   emacs-know-your-http-well
   emacs-macrostep
   emacs-magit-org-todos-el
   emacs-mbsync
   emacs-move-text
   emacs-org-mind-map       ; General mind maps from Org files
   emacs-redshift
   emacs-scratch-el
   emacs-terminal-here-checkout))

(define guix-wigust-packages
  (append
   (list

    pulsemixer-emacs-keybindings
    emacs-beginend

    emacs-edit-server ; See <https://github.com/stsquad/emacs_chrome/>.

    python-starred ; Fetch a list of stars from GitHub user

    emacs-academic-phrases
    emacs-atomic-chrome
    emacs-awk-it
    emacs-debpaste           ; Front end to <https://paste.debian.net/>
    emacs-edit-indirect
    emacs-emms-player-simple-mpv ; Frontend to MPV for Emms
    emacs-epkg
    emacs-esup
    emacs-f3
    emacs-fancy-narrow
    emacs-helm-emms
    emacs-helm-gtags
    emacs-helm-navi
    emacs-lice-el
    emacs-outshine           ; Emacs outline-mode
    emacs-pcre2el
    emacs-perl-live
    emacs-suggest
    emacs-validate

    vc-dwim-git-worktree

    wrapper-xclip)

   guix-wigust-packages-emacs))
