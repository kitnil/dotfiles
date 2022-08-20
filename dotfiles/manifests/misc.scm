(use-modules (wigust packages databases)
	     (wigust packages ruby)
	     (gnu packages chromium))

(list
 emacs-anywhere-mode
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
 ruby-gitlab
 dynamips ;gns3 requirement
 ungoogled-chromium

 chicken go m4 racket perl python python-hy r

 mercurial gource
 
 colormake

 guile-commonmark ; Commonmark for Guile
 ;; gwl              ; Guix workflow management ; fails to build
 haunt            ; Guile static site generator
 )
