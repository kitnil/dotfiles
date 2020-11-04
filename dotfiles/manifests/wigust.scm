(use-modules (gnu packages emacs)
             (wigust packages databases)
             (wigust packages admin)
             (wigust packages documentation)
             (wigust packages dotfiles)
             (wigust packages emacs)
             (wigust packages games)
             (wigust packages guix)
             (wigust packages password-utils)
             (wigust packages pspg)
             (wigust packages pulseaudio)
             (wigust packages python)
             (wigust packages ruby)
             (wigust packages virtualization)
             (wigust packages video)
             (wigust packages xdisorg)
             (wigust packages xorg))

(define guix-wigust-packages-emacs
  (list
   emacs-anywhere-mode
   emacs-apache-mode
   emacs-atomic-chrome
   emacs-awk-it
   emacs-dorg
   emacs-ytel
   emacs-copy-as-format
   emacs-stupid-indent-mode
   ;; emacs-dashboard-with-agenda-reverse
   emacs-engine-mode-autoload ; Define searches on websites
   emacs-elfeed-score
   emacs-eval-in-repl       ; Evaluate to different Repls
   emacs-flyspell-correct
   emacs-git-messenger-diff-mode
   emacs-gited
   emacs-guix-misc
   emacs-helm-pass-custom-dmenu ; Front end to password-store
   emacs-highlight-indent-guides
   emacs-hydra-timestamp
   emacs-info-colors        ; Colorize info pages
   emacs-mediawiki
   emacs-lsp-java-fix-treemacs
   emacs-org-tanglesync-1.1.0
   emacs-perl-live
   emacs-psysh
   emacs-redshift
   emacs-slack-patched
   emacs-strace-mode-special ; Colorize `strace' logs
   emacs-terminal-here-checkout
   emacs-plain-org-wiki
   emacs-wi-utils
   ))

(packages->manifest
 (cons*

  autopostgresqlbackup

  y2rss

  ;; TODO: Fix pulsemixer-emacs-keybindings

  ruby-gitlab

  python-starred ; Fetch a list of stars from GitHub user

  ok-sh
  git-quick-stats

  guix-browse
  guix-latest-eval
  guix-misc

  wrapper-xclip

  dynamips ;gns3 requirement

  python-open-with

  xterm-my
  ;; qemu-my

  quickwall

  pspg ; SQL pager

  slides-concise-gnu-bash
  slides-devops-world-jenkins-casc
  slides-linux-perf-tools
  documentation-arcconf

  git-splits

  password-store-custom-dmenu ; Password management

  dotfiles

  qemu-windows10

  guix-wigust-packages-emacs))
