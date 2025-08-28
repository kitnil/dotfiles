(define-module (wugi utils package)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (package-from-program-file))

(define* (package-from-program-file program #:optional (location "/bin"))
  (package
    (name (program-file-name program))
    (version "0.0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (mkdir-p (string-append #$output #$location))
          (copy-file #$program
                     (string-append %output #$location "/" #$(program-file-name program)))
          (chmod (string-append %output #$location "/" #$(program-file-name program))
                 #o555)
          #t)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))
