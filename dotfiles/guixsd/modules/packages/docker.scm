(define-module (packages docker)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1))

(define-public docker-guix-workstation
  (package
    (name "docker-guix-workstation")
    (version "0.0.1")
    (source (local-file "/home/oleg/.local/share/chezmoi/dotfiles/docker/guix-workstation/run.sh"))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:builder
      #~(begin
          (mkdir %output)
          (mkdir (string-append %output "/bin"))
          (copy-file #$(this-package-native-input "source")
                     (string-append %output "/bin/" #$name))
          (chmod (string-append %output "/bin/" #$name)
                 #o555)
          #t)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))
