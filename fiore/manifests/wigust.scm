(define-module (fiore manifests wigust)
  #:use-module (wigust packages cdrom)
  #:use-module (wigust packages emacs)
  #:use-module (wigust packages pulseaudio)
  #:use-module (wigust packages python)
  #:use-module (wigust packages version-control)
  #:use-module (wigust packages ruby)
  #:use-module (local packages emacs)
  #:use-module (local packages guix)
  #:use-module (local packages xdisorg)
  #:use-module (gnu packages emacs)
  #:export (guix-wigust-packages
            guix-wigust-packages-emacs
            guix-wigust-packages-local))

(define guix-wigust-packages-emacs
  (list
   emacs-athena
   emacs-engine-mode-autoload ; Define searches on websites
   emms-next
   emacs-strace-mode-special ; Colorize `strace' logs
   emacs-anywhere-mode
   emacs-emamux
   emacs-eval-in-repl       ; Evaluate to different Repls
   emacs-git-messenger-diff-mode
   emacs-helm-c-yasnippet
   emacs-helm-pass          ; Front end to password-store
   emacs-info-colors        ; Colorize info pages
   emacs-redshift
   emacs-ivy-yasnippet
   emacs-scratch-el
   emacs-terminal-here-checkout
   emacs-flyspell-correct
   emacs-hydra-timestamp
   emacs-mediawiki))

(define guix-wigust-packages-local
  (list

   emacs-guix-misc
   guix-browse
   guix-latest-eval
   guix-misc

   ))

(define guix-wigust-packages
  (append
   (list

    cdrkit-libre

    pulsemixer-emacs-keybindings

    ruby-gitlab
    python-starred ; Fetch a list of stars from GitHub user

    emacs-atomic-chrome
    emacs-awk-it
    emacs-debpaste           ; Front end to <https://paste.debian.net/>
    emacs-edit-indirect
    emacs-epkg
    emacs-helm-navi
    emacs-pcre2el
    emacs-perl-live

    vc-dwim-git-worktree

    wrapper-xclip)

   guix-wigust-packages-emacs
   guix-wigust-packages-local))
