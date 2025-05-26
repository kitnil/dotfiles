(define-module (wugi manifests misc)
  #:use-module (gnu packages chromium)
  #:export (%misc-manifest))

(define (%misc-manifest)
  (list emacs-anywhere-mode
        emacs-atomic-chrome
        emacs-awk-it
        emacs-debpaste
        ;; emacs-eval-in-repl
        emacs-flyspell-correct
        emacs-hydra-timestamp
        emacs-hydra-timestamp
        emacs-mediawiki
        emacs-pcre2el
        emacs-perl-live
        emacs-psysh
        emacs-redshift
        ;; ruby-gitlab
        dynamips ;gns3 requirement
        ungoogled-chromium

        chicken go m4 perl python python-hy r

        mercurial gource

        guile-commonmark ; Commonmark for Guile
        ;; gwl           ; Guix workflow management ; fails to build
        haunt            ; Guile static site generator

        colormake))
