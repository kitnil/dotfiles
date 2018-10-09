(define-module (fiore manifests wigust)
  #:use-module (chromium chromium)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages wigust-emacs)
  #:use-module (gnu packages wigust-guix)
  #:use-module (gnu packages wigust-pulseaudio)
  #:use-module (gnu packages wigust-python)
  #:use-module (gnu packages wigust-ruby)
  #:use-module (gnu packages wigust-version-control)
  #:use-module (gnu packages wigust-xdisorg)
  #:export (guix-wigust-packages
            guix-wigust-packages-emacs))

(define guix-wigust-packages-emacs
  (list
   emacs-anywhere-mode
   emacs-athena
   emacs-atomic-chrome
   emacs-awk-it
   emacs-debpaste           ; Front end to <https://paste.debian.net/>
   emacs-edit-indirect
   emacs-emamux
   emacs-engine-mode-autoload ; Define searches on websites
   emacs-epkg
   emacs-eval-in-repl       ; Evaluate to different Repls
   emacs-flyspell-correct
   emacs-git-messenger-diff-mode
   emacs-gited
   emacs-guix-misc
   emacs-helm-c-yasnippet
   emacs-helm-navi
   emacs-helm-pass          ; Front end to password-store
   emacs-hydra-timestamp
   emacs-info-colors        ; Colorize info pages
   emacs-ivy-yasnippet
   emacs-mediawiki
   emacs-pcre2el
   emacs-perl-live
   emacs-psysh
   emacs-redshift
   emacs-scratch-el
   emacs-strace-mode-special ; Colorize `strace' logs
   emacs-terminal-here-checkout
   ))

(define guix-wigust-packages
  (append
   (list

    chromium

    pulsemixer-emacs-keybindings

    ruby-gitlab
    python-starred ; Fetch a list of stars from GitHub user

    guix-browse
    guix-latest-eval
    guix-misc

    vc-dwim-git-worktree

    wrapper-xclip)

   guix-wigust-packages-emacs))
