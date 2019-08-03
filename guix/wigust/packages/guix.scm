(define-module (wigust packages guix)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages autotools)
  #:use-module (wigust packages emacs)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public guix-browse
  (let ((commit "83022c91f16141514282e53ec94373d9c9cc1dca"))
    (package
      (name "guix-browse")
      (version (git-version "0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/wigust/guix-browse")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1rxsjalb86lkw0qimqmr3yz65pc817yma2pynd9cn1a0nni138ll"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules
         ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (let ((directory "guix/scripts"))
             (install-file (string-append directory "/browse.scm")
                           (string-append %output "/share/guile/site/2.2/"
                                          directory)))
           #t)))
      (home-page #f)
      (synopsis #f)
      (description #f)
      (license #f))))

(define-public guix-latest-eval
  (let ((commit "819c3fd9746ecda6a9fd005d1dd7efc70944d2c1"))
    (package
      (name "guix-latest-eval")
      (version (string-append "0.0.1" "-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/wigust/guix-latest-eval")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1nnz8qfmg3f6vn2gc9g3k535smfwb50yq2d1snf0nf9mcn1yji7b"))))
      (build-system trivial-build-system)
      (inputs
       `(("bash" ,bash)
         ("curl" ,curl)))
      (arguments
       `(#:modules
         ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (setenv "PATH" (string-append
                           (assoc-ref %build-inputs "bash") "/bin" ":"
                           (assoc-ref %build-inputs "curl") "/bin" ":"))
           (substitute* "guix-latest-eval"
             (("/bin/sh") (which "bash"))
             (("curl") (which "curl")))
           (let ((directory "bin"))
             (install-file "guix-latest-eval"
                           (string-append %output "/" directory)))
           #t)))
      (home-page #f)
      (synopsis "Get the most recent fully evaluated commit from the build farm")
      (description
       "This package provides a Curl script to get the most recent fully
evaluated commit from the build farm.")
      (license license:gpl3+))))

(define-public guix-misc
  (let ((commit "9d10a3ceeaa538e69c2ee8084fb7d4fc39d87dd3"))
    (package
      (name "guix-misc")
      (version (git-version "0.0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/wigust/guix-misc.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0hilnzkxlm66hw8dzpa61ahay38r5ppcfamwc8m8mdbg9kkd65nz"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (setenv "PATH" (string-append
                           (assoc-ref %build-inputs "bash") "/bin" ":"
                           (assoc-ref %build-inputs "parallel") "/bin" ":"
                           (assoc-ref %build-inputs "findutils") "/bin" ":"
                           (assoc-ref %build-inputs "coreutils") "/bin" ":"
                           (assoc-ref %build-inputs "guile") "/bin" ":"))
           (with-directory-excursion "scripts"
             (let ((file "guix-compile-package-path"))
               (substitute* file
                 (("/bin/sh") (which "bash"))
                 (("@FIND_BIN@") (which "find"))
                 (("@GUILD_BIN@") (which "guild"))
                 (("@PARALLEL_BIN@") (which "parallel"))
                 (("@PRINTENV_BIN@") (which "printenv"))
                 (("@TR_BIN@") (which "tr")))
               (chmod file #o555)
               (install-file file (string-append %output "/bin"))))
           #t)))
      (inputs
       `(("bash" ,bash)
         ("coreutils" ,coreutils)
         ("findutils" ,findutils)
         ("guile" ,guile-2.2)
         ("parallel" ,parallel)))
      (home-page "https://gitlab.com/wigust/guix-misc")
      (synopsis "Additional programs to control Guix")
      (description
       "This package provides an additional programs to control Guix.")
      (license license:gpl3+))))
