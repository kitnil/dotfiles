(define-module (packages monitoring)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
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
