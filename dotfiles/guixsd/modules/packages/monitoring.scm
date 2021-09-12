(define-module (packages monitoring)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) #:prefix license:))

(define-public karma
  (package
    (name "karma")
    (version "0.91")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/prymitive/karma/releases/download/v"
                                  version "/karma-linux-amd64.tar.gz"))
              (sha256
               (base32
                "10wakk41pz79837v2j58158f38xqvbhf5vh67crhdrswi2w0k4ch"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/bin"))
         (setenv "PATH" (string-append
                         (assoc-ref %build-inputs "tar") "/bin" ":"
                         (assoc-ref %build-inputs "gzip") "/bin"))
         (invoke "tar" "xf" (assoc-ref %build-inputs "source")
                 "-C" (string-append %output "/bin")))))
    (home-page "https://github.com/prymitive/")
    (synopsis "Front-end to Prometheus Alertmanager")
    (description
     "This package provides a Karma front-end to Prometheus Alertmanager.")
    (license license:expat)))
