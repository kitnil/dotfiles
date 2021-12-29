(define-module (packages monitoring)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
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

(define-public prometheus-blackbox-exporter
  (package
    (name "prometheus-blackbox-exporter")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/prometheus/blackbox_exporter/releases/download/v"
         version "/blackbox_exporter-" version ".linux-amd64.tar.gz"))
       (sha256
        (base32
         "15h84k92aj65nqgrj9g625f12y5cmhfhwlf15ab4d6sg9hwy2amg"))))
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
         (invoke "tar" "--strip-components=1"
                 "-xf" (assoc-ref %build-inputs "source"))
         (install-file "blackbox_exporter"
                       (string-append %output "/bin")))))
    (build-system trivial-build-system)
    (home-page "https://github.com/prometheus/blackbox_exporter")
    (synopsis "Blackbox prober for Prometheus")
    (description "The blackbox exporter allows blackbox probing of network
endpoints over HTTP, HTTPS, DNS, TCP and ICMP.  Additional modules can be
defined to suit other needs.

Querying of endpoints happens via HTTP GET queries, by specifying the target
name and what kind of probing to execute. Results from the probe are returned
as a set of Prometheus metrics.")
    (license license:expat)))

(define-public prometheus-bird-exporter
  (package
    (name "prometheus-bird-exporter")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/czerwonk/bird_exporter/releases/download/"
             version "/bird_exporter_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32
         "1d29gk0705966m7x2dch1pw41dnnskx29z90fzn53kry1i8fw1wq"))))
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
         (invoke "tar" "-xf" (assoc-ref %build-inputs "source"))
         (install-file "bird_exporter"
                       (string-append %output "/bin")))))
    (home-page "https://github.com/czerwonk/bird_exporter/")
    (synopsis "Bird protocol state exporter for bird routing daemon")
    (description "Metric exporter for bird routing daemon to use with
Prometheus.")
    (license license:expat)))

(define-public prometheus-smartctl-exporter
  (package
    (name "prometheus-smartctl-exporter")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/prometheus-community/smartctl_exporter/releases/download/smartctl_exporter_"
         version "/smartctl_exporter"))
       (sha256
        (base32
         "11qr4p7dwps6la9rv09gdmmri7ziap69agaggr8w787dapi08g50"))))
    (build-system trivial-build-system)
    (inputs
     `(("glibc" ,glibc)
       ("patchelf" ,patchelf)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/bin"))
         (setenv "PATH"
                 (string-append
                  (assoc-ref %build-inputs "patchelf") "/bin"))
         (copy-file (assoc-ref %build-inputs "source") "smartctl_exporter")
         (chmod "smartctl_exporter" #o755)
         (invoke "patchelf" "--set-interpreter"
                 (string-append (assoc-ref %build-inputs "glibc")
                                "/lib/ld-linux-x86-64.so.2")
                 "smartctl_exporter")
         (chmod "smartctl_exporter" #o555)
         (let ((bin (string-append %output "/bin")))
           (mkdir-p bin)
           (install-file "smartctl_exporter" bin)))))
    (home-page "https://github.com/prometheus-community/smartctl_exporter")
    (synopsis "Export smartctl statistics to Prometheus")
    (description "This package provides a smartctl exporter for Prometheus.")
    (license license:expat)))

(define-public prometheus-exim-exporter
  (package
    (name "prometheus-exim-exporter")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/gvengel/exim_exporter/releases/download/v"
             version "/exim_exporter"))
       (sha256
        (base32
         "0q9pczzb7sayhfa66a2vf72fbzgnn3j1ygwd06b059xfx1y3aadx"))))
    (build-system trivial-build-system)
    (inputs
     `(("glibc" ,glibc)
       ("patchelf" ,patchelf)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/bin"))
         (setenv "PATH"
                 (string-append
                  (assoc-ref %build-inputs "patchelf") "/bin"))
         (copy-file (assoc-ref %build-inputs "source") "exim_exporter")
         (chmod "exim_exporter" #o755)
         (invoke "patchelf" "--set-interpreter"
                 (string-append (assoc-ref %build-inputs "glibc")
                                "/lib/ld-linux-x86-64.so.2")
                 "exim_exporter")
         (chmod "exim_exporter" #o555)
         (let ((bin (string-append %output "/bin")))
           (mkdir-p bin)
           (install-file "exim_exporter" bin)))))
    (home-page "https://github.com/gvengel/exim_exporter")
    (synopsis "Exim metrics exporter for Prometheus")
    (description "Exports metrics from the exim mail server for consumption by
Prometheus.")
    (license license:expat)))

(define-public prometheus-ssh-exporter
  (package
    (name "prometheus-ssh-exporter")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/treydock/ssh_exporter/releases/download/v"
             version "/ssh_exporter-" version ".linux-amd64.tar.gz"))
       (sha256
        (base32
         "1cba96a08bv3phsvp5ngrl8pkqgjx7mjpr1xjvs5aicdigzbbly7"))))
    (build-system trivial-build-system)
    (inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/bin"))
         (setenv "PATH"
                 (string-append
                  (assoc-ref %build-inputs "gzip") "/bin"
                  ":" (assoc-ref %build-inputs "tar") "/bin"))
         (invoke "tar" "--strip-components=1" "-xf"
                 (assoc-ref %build-inputs "source"))
         (let ((bin (string-append %output "/bin")))
           (mkdir-p bin)
           (install-file "ssh_exporter" bin)))))
    (home-page "https://github.com/gvengel/ssh_exporter")
    (synopsis "Ssh metrics exporter for Prometheus")
    (description "Exports metrics from the SSH for consumption by
Prometheus.")
    (license license:expat)))
