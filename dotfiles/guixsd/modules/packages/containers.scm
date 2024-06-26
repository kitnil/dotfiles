(define-module (packages containers)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary))

(define-public nerdctl
  (package
    (name "nerdctl")
    (version "1.7.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/containerd/nerdctl/releases/download/v"
         version "/nerdctl-" version "-linux-amd64.tar.gz"))
       (sha256
        (base32
         "1w700wjv41lxq8fg4300pqyl69r7qv5shylxxl2c3idy5njdc9h3"))))
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

(define-public crictl
  (package
    (name "crictl")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/kubernetes-sigs/cri-tools/releases/download/v"
                                  version "/crictl-v"
                                  version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "1vaf2g22zmp3zqjxdb9zfkl06pjnwc7vmzqhh5a6p7zi7caf59fd"))))
    (build-system trivial-build-system)
    (native-inputs (list source gzip tar))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 popen)
                       (ice-9 rdelim))
          (let ((bin (string-append #$output "/bin")))
            (mkdir-p (string-append #$output "/bin"))
            (setenv "PATH" (string-append
                            #$(this-package-native-input "tar") "/bin" ":"
                            #$(this-package-native-input "gzip") "/bin"))
            (invoke "tar"
                    "-xf" (assoc-ref %build-inputs "source")
                    "-C" bin
                    "crictl")
            ;; Install shell completion.
            (let ((bash (string-append #$output "/etc/bash_completion.d")))
              (mkdir-p bash)
              (call-with-output-file (string-append bash "/crictl")
                (lambda (port)
                  (display
                   (let* ((port (open-pipe* OPEN_READ (string-append #$output "/bin/crictl")
                                            "completion" "bash"))
                          (output (read-string port)))
                     (close-port port)
                     (string-trim-right output #\newline))
                   port))))))))
    (home-page "https://github.com/kubernetes-sigs/cri-tools")
    (synopsis "")
    (description "")
    (license #f)))

(define-public diffoci
  (package
    (name "diffoci")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/reproducible-containers/diffoci/releases/download/v"
                                  version "/diffoci-v" version ".linux-amd64"))
              (sha256
               (base32
                "1f0jr0jigk4kj1a1jhxwzjls82f483z7376l1z3czbn949361nkg"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'move-binary-file-and-cleanup
            (lambda _
              (delete-file (string-append #$output "/environment-variables"))
              (mkdir-p (string-append #$output "/bin"))
              (rename-file (string-append #$output "/diffoci-v" #$version ".linux-amd64")
                           (string-append #$output "/bin/diffoci"))
              (chmod (string-append #$output "/bin/diffoci") #o555)))
          (add-after 'move-binary-file-and-cleanup 'install-shell-completions
            (lambda _
              (use-modules (ice-9 popen)
                           (ice-9 rdelim))
              (let ((bash (string-append #$output "/etc/bash_completion.d")))
                (mkdir-p bash)
                (call-with-output-file (string-append bash "/diffoci")
                  (lambda (port)
                    (display
                     (let* ((port (open-pipe* OPEN_READ (string-append #$output "/bin/diffoci")
                                              "completion" "bash"))
                            (output (read-string port)))
                       (close-port port)
                       (string-trim-right output #\newline))
                     port)))))))))
    (home-page "https://github.com/reproducible-containers/diffoci")
    (synopsis "Show diff for Docker and OCI container images")
    (description "This package provides a program to show diff for Docker and
OCI container images")
    (license license:asl2.0)))
