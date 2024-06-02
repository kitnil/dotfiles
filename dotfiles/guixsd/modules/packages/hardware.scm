(define-module (packages hardware)
  #:use-module (guix build utils)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim))

(define-public ddcutil-daemon
  (package
    (name "ddcutil-daemon")
    (version "0.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://cgit.wugi.info/wigust/ddcutil-daemon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01wvg2qwhga17z9wrxi5sij2cs3nsmgcsfcwzlxgdd5pkzikir7j"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/kitnil/ddcutil-daemon"))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))
