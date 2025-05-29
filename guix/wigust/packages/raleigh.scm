(define-module (wigust packages raleigh)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages))

(define-public raleigh-reloaded-theme
  (let ((commit "65c5a31c539dbcd2ab0a277e6352269567d8934a")
        (revision "1"))
    (package
      (name "raleigh-reloaded-theme")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/vlastavesely/raleigh-reloaded.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1mgq1k8bl8qglmxsyj729srcgfc7bjbas18xcjv1c9kbrxpbsrai"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" %output))
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           ;; Only Makefile.
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "src" (string-append
                                        (assoc-ref outputs "out")
                                        "/share/themes/Raleigh-Reloaded")))))))
      (home-page "https://github.com/vlastavesely/raleigh-reloaded")
      (synopsis "A GTK theme aiming to revive the classic Raleigh theme")
      (description "This is a GTK-3.20 theme aiming to revive the classic
Raleigh theme.  It is a minimal theme written from scratch completely.  For
that reason, some elements may be unstyled and therefore look ugly.")
      (license license:gpl2+))))
