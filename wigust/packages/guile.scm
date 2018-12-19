(define-module (wigust packages guile)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils))

(define-public haunt-symlinks
  (package
    (inherit haunt)
    (name "haunt-symlinks")
    (source
     (origin
       (inherit (package-source haunt))
       (patches (search-patches "haunt-asset-follow-symlinks.patch"))))))

(define-public guile-feed
  (let ((commit "ce1dd1a9f7c0eaeccfd05b328fc254afbfb84e44"))
    (package
      (name "guile-feed")
      (version (git-version "0.0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://cgit.duckdns.org/git/guile-feed")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "08aq6ibm7yxrc0slwf3hwk5hxw0ri2dcz6v8bhlnypnf1bim44c5"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules
         ((guix build gnu-build-system)
          (guix build utils)
          (srfi srfi-26)
          (ice-9 popen)
          (ice-9 rdelim))
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Make sure the 'guix' command finds GnuTLS,
               ;; Guile-JSON, and Guile-Git automatically.
               (let* ((out    (assoc-ref outputs "out"))
                      (guile  (assoc-ref inputs "guile"))
                      (gcrypt (assoc-ref inputs "guile-gcrypt"))
                      (gnutls (assoc-ref inputs "gnutls"))
                      (guix   (assoc-ref inputs "guix"))
                      (json   (assoc-ref inputs "guile-json"))
                      (deps   (list gcrypt gnutls guix json out))
                      (effective
                       (read-line
                        (open-pipe* OPEN_READ
                                    (string-append guile "/bin/guile")
                                    "-c" "(display (effective-version))")))
                      (path   (string-join
                               (map (cut string-append <>
                                         "/share/guile/site/"
                                         effective)
                                    deps)
                               ":"))
                      (gopath (string-join
                               (map (cut string-append <>
                                         "/lib/guile/" effective
                                         "/site-ccache")
                                    deps)
                               ":")))

                 (wrap-program (string-append out "/bin/feed")
                   `("GUILE_LOAD_PATH" ":" prefix (,path))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,gopath)))

                 #t))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("gnutls" ,gnutls)
         ("guile" ,guile-2.2)
         ("guile-gcrypt" ,guile-gcrypt)
         ("guile-json" ,guile-json)
         ("guix" ,guix)))
      (home-page "https://gitlab.com/wigust/guile-feed")
      (synopsis "Command-line RSS feeds manager")
      (description
       "This package provides a command-line program to manage RSS feeds
written in Guile.")
      (license license:gpl3+))))
