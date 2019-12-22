(use-modules (gnu packages emacs)
             (wigust packages databases)
             (wigust packages admin)
             (wigust packages emacs)
             (wigust packages games)
             (wigust packages guix)
             (wigust packages pulseaudio)
             (wigust packages python)
             (wigust packages ruby)
             (wigust packages version-control)
             (wigust packages virtualization)
             (wigust packages xdisorg)
             (wigust packages xorg))

(define guix-wigust-packages-emacs
  (list
   emacs-anywhere-mode
   emacs-apache-mode
   emacs-atomic-chrome
   emacs-awk-it
   emacs-copy-as-format
   emacs-engine-mode-autoload ; Define searches on websites
   emacs-eval-in-repl       ; Evaluate to different Repls
   emacs-flyspell-correct
   emacs-git-messenger-diff-mode
   emacs-gited
   emacs-guix-misc
   emacs-highlight-indent-guides
   emacs-hydra-timestamp
   emacs-info-colors        ; Colorize info pages
   emacs-mediawiki
   emacs-perl-live
   emacs-psysh
   emacs-redshift
   emacs-slack-patched
   emacs-strace-mode-special ; Colorize `strace' logs
   emacs-terminal-here-checkout
   emacs-plain-org-wiki
   emacs-tramp-auto-auth-my
   ))

(packages->manifest
 (cons*

  autopostgresqlbackup

  pulsemixer-emacs-keybindings

  ruby-gitlab
  python-starred ; Fetch a list of stars from GitHub user
  ok-sh
  git-quick-stats

  guix-browse
  guix-latest-eval
  guix-misc

  vc-dwim-git-worktree

  wrapper-xclip

  dynamips ;gns3 requirement

  python-open-with

  xterm-my
  ;; qemu-my

  tome4-with-addons
  quickwall
  
  guix-wigust-packages-emacs))

