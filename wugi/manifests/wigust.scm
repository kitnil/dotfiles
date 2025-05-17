(define-module (wugi manifests wigust)
  #:use-module (guix profiles)
  #:use-module (gnu packages emacs)
  #:use-module (wigust packages databases)
  #:use-module (wigust packages admin)
  #:use-module (wigust packages documentation)
  #:use-module (wigust packages dotfiles)
  #:use-module (wigust packages emacs)
  #:use-module (wigust packages games)
  #:use-module (wigust packages guix)
  #:use-module (wigust packages pspg)
  #:use-module (wigust packages pulseaudio)
  #:use-module (wigust packages python)
  #:use-module (wigust packages ruby)
  #:use-module (wigust packages virtualization)
  #:use-module (wigust packages video)
  #:use-module (wigust packages vnc)
  #:use-module (wigust packages telegram)
  #:use-module (wigust packages xdisorg)
  #:use-module (wigust packages xorg)
  #:export (%wigust-manifest))

(define (%wigust-manifest)
  (define guix-wigust-packages-emacs
    (list emacs-anywhere-mode
          emacs-apache-mode
          emacs-atomic-chrome
          emacs-awk-it
          emacs-dorg
          emacs-ytel
          emacs-logstash
          emacs-copy-as-format
          emacs-stupid-indent-mode
          ;; emacs-dashboard-with-agenda-reverse
          emacs-engine-mode-autoload ; Define searches on websites
          ;; emacs-eval-in-repl       ; Evaluate to different Repls
          emacs-flyspell-correct
          emacs-git-messenger-diff-mode
          emacs-gited
          emacs-guix-misc
          emacs-highlight-indent-guides
          emacs-hydra-timestamp
          emacs-info-colors        ; Colorize info pages
          emacs-mediawiki
          emacs-org-tanglesync-1.1.0
          emacs-perl-live
          emacs-psysh
          emacs-redshift
          emacs-strace-mode-special ; Colorize `strace' logs
          emacs-terminal-here-checkout
          emacs-thesaurus
          emacs-plain-org-wiki
          emacs-wi-utils
          emacs-yaml-pro))

  (packages->manifest
   (append (list ok-sh
                 git-quick-stats

                 ;; y2rss

                 ;; TODO: Fix pulsemixer-emacs-keybindings

                 ;; ruby-gitlab

                 ;; XXX: Failed to build python-starred ; Fetch a list of stars from GitHub user

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
                 cheatsheet-gdb

                 git-splits

                 dotfiles

                 qemu-windows10)
           guix-wigust-packages-emacs)))
