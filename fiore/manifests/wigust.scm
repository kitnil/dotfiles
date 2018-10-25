(use-modules (chromium chromium)
             (gnu packages emacs)
             (gnu packages wigust-emacs)
             (gnu packages wigust-guix)
             (gnu packages wigust-pulseaudio)
             (gnu packages wigust-python)
             (gnu packages wigust-ruby)
             (gnu packages wigust-version-control)
             (gnu packages wigust-xdisorg))

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

(packages->manifest
 (cons*

  chromium

  pulsemixer-emacs-keybindings

  ruby-gitlab
  python-starred ; Fetch a list of stars from GitHub user

  guix-browse
  guix-latest-eval
  guix-misc

  vc-dwim-git-worktree

  wrapper-xclip

  guix-wigust-packages-emacs))

