(define-module (packages containers)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression))

(define-public nerdctl
  (package
    (name "nerdctl")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/containerd/nerdctl/releases/download/v"
         version "/nerdctl-" version "-linux-amd64.tar.gz"))
       (sha256
        (base32
         "1qx012zcgvz42sjck9j0xp9qzg8r7aimp6csg638swp42nbg852y"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("source" ,source)
       ("gzip" ,gzip)
       ("tar" ,tar)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 popen)
                       (ice-9 rdelim))
          (let ((bin (string-append #$output "/bin")))
            ;; Install executables.
            (mkdir-p (string-append %output "/bin"))
            (setenv "PATH" (string-append
                            (assoc-ref %build-inputs "tar") "/bin" ":"
                            (assoc-ref %build-inputs "gzip") "/bin"))
            (invoke "tar"
                    "-xf" (assoc-ref %build-inputs "source")
                    "-C" (string-append %output "/bin"))
            ;; Install shell completion.
            (let ((bash (string-append %output "/etc/bash_completion.d")))
              (mkdir-p bash)
              (call-with-output-file (string-append bash "/nerdctl")
                (lambda (port)
                  (display
                   (let* ((port (open-pipe* OPEN_READ (string-append %output "/bin/nerdctl")
                                            "completion" "bash"))
                          (output (read-string port)))
                     (close-port port)
                     (string-trim-right output #\newline))
                   port))))))))
    (home-page "https://github.com/containerd/nerdctl")
    (synopsis "Docker-compatible CLI for containerd")
    (description "nerdctl is a Docker-compatible CLI for containerd.")
    (license license:asl2.0)))
