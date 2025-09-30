(define-module (wugi packages kubernetes)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system go)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rsync))

(define-public k3s
  (package
    (name "k3s")
    (version "1.26.13")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/k3s-io/k3s/releases/download/v"
                       version "%2Bk3s1/k3s"))
       (sha256
        (base32
         "1p43q5b2qgzrr4kybcqd42jccvyg4n322b8q3xmczd25dhr7wdlf"))))
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

(define-public virtctl
  (package
    (name "virtctl")
    (version "0.58.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/kubevirt/kubevirt/releases/download/v"
                       version "/virtctl-v" version "-linux-amd64"))
       (sha256
        (base32
         "1vv11525ggdwrc14lap6vmz0c2bch2bgjlx8zpygbsyzxsl6zpap"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bin (string-append #$output "/bin"))
                 (virtctl (string-append bin "/virtctl")))
            (mkdir-p bin)
            (copy-file #$(this-package-native-input "source") virtctl)
            (chmod virtctl #o555)))))
    (home-page "https://virtctl.io/")
    (synopsis "Kubevirt virtual machines control utility")
    (description
     "This package provides a Kubevirt virtual machines control utility.")
    (license license:asl2.0)))

(define-public cilium
  (package
    (name "cilium")
    (version "0.15.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cilium/cilium-cli/releases/download/v"
                                  version "/cilium-linux-amd64.tar.gz"))
              (sha256
               (base32
                "1nl4qdbgh9ffv16abpapnqfaisl774n9xskw18jil6g21b2g38yd"))))
    (build-system trivial-build-system)
    (native-inputs (list source gzip tar glibc patchelf))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bin (string-append #$output "/bin"))
                 (cilium (string-append bin "/cilium")))
            (setenv "PATH" (string-append
                            #$(this-package-native-input "tar") "/bin" ":"
                            #$(this-package-native-input "gzip") "/bin" ":"))
            (invoke "tar"
                    "-xf" (assoc-ref %build-inputs "source")
                    "cilium")
            (mkdir-p (string-append #$output "/bin"))
            (copy-file "cilium" (string-append #$output "/bin/cilium"))
            (chmod cilium #o555)))))
    (home-page "Cilium")
    (synopsis "Cilium")
    (description "")
    (license #f)))

(define-public edgecore
  (package
    (name "edgecore")
    (version "1.17.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/kubeedge/kubeedge/releases/download/v"
                                  version "/kubeedge-v" version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "1xgsiccr2ww818kz4c1apzcbj0bh84aifvx2glrmxhzdyaar8qh4"))))
    (build-system trivial-build-system)
    (native-inputs (list source gzip tar))
    (inputs (list glibc patchelf))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 popen)
                       (ice-9 rdelim))
          (let* ((bin (string-append #$output "/bin"))
                 (edgecore (string-append bin "/edgecore")))
            (setenv "PATH" (string-append
                            #$(this-package-native-input "tar") "/bin" ":"
                            #$(this-package-native-input "gzip") "/bin" ":"
                            #$(this-package-input "patchelf") "/bin"))
            (invoke "tar"
                    "--strip-components=2"
                    "-xf" (assoc-ref %build-inputs "source")
                    "kubeedge-v1.17.0-linux-amd64/edge/edgecore")
            (mkdir-p (string-append #$output "/bin"))
            (invoke "patchelf" "--set-interpreter"
                    (string-append #$(this-package-input "glibc")
                                   "/lib/ld-linux-x86-64.so.2")
                    "edgecore")
            (copy-file "edgecore" (string-append #$output "/bin/edgecore"))
            (chmod edgecore #o555)
            ;; Install shell completion.
            (let ((bash (string-append #$output "/etc/bash_completion.d")))
              (mkdir-p bash)
              (call-with-output-file (string-append bash "/edgecore")
                (lambda (port)
                  (display
                   (let* ((port (open-pipe* OPEN_READ (string-append #$output "/bin/edgecore")
                                            "completion" "bash"))
                          (output (read-string port)))
                     (close-port port)
                     (string-trim-right output #\newline))
                   port))))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

