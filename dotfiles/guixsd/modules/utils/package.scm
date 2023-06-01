(define-module (utils package)
  #:use-module (gnu packages)
  #:use-module (gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (package-from-program-file))

(define (package-from-program-file program)
  (package
    (name (program-file-name program))
    (version "0.0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      #~(begin
          (mkdir %output)
          (mkdir (string-append %output "/bin"))
          (copy-file #$program
                     (string-append %output "/bin/" #$(program-file-name program)))
          (chmod (string-append %output "/bin/" #$(program-file-name program))
                 #o555)
          #t)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))
