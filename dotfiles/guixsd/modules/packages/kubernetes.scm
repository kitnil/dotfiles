(define-module (packages kubernetes)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages python-xyz))

(define-public k3s
  (package
    (name "k3s")
    (version "1.23.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/k3s-io/k3s/releases/download/v"
                       version "%2Bk3s1/k3s"))
       (sha256
        (base32
         "1i7rv1pj29w1rrbf7q5r9bkxhvv1hphki5x7979z1wps628h63m6"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bin (string-append #$output "/bin"))
                 (k3s (string-append bin "/k3s")))
            (mkdir-p bin)
            (copy-file #$(this-package-native-input "source") k3s)
            (chmod k3s #o555)))))
    (home-page "https://k3s.io/")
    (synopsis "Lightweight Kubernetes")
    (description
     "The certified Kubernetes distribution built for IoT & Edge computing")
    (license license:asl2.0)))
