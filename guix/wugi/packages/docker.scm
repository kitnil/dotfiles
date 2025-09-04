(define-module (wugi packages docker)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define %source-dir
  (string-append %home "/src/cgit.wugi.info/wigust/dotfiles"))

(define-public docker-guix-workstation
  (package
    (name "docker-guix-workstation")
    (version "0.0.1")
    (source
     (local-file
      ;; TODO: Use %source-dir.
      (string-append "/home/user" "/src/cgit.wugi.info/wigust/dotfiles"
                     "/dotfiles/docker/guix-workstation/run.sh")))
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
