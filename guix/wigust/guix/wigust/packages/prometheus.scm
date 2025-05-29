(define-module (wigust packages prometheus)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:))

(define-public go-github-com-prometheus-prometheud-cmd-prometheus
  (package
    (name "go-github-com-prometheus-prometheus-cmd-prometheus")
    (version "2.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/prometheus/prometheus.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08nd88m162bw5612cvw5fl028l2n9jy1v4w2a8cyd0dj4lxs5d98"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/prometheus/prometheus/cmd/prometheus"
       #:unpack-path "github.com/prometheus/prometheus"))
    (synopsis "Monitoring system & time series database")
    (description "Prometheus is an open-source monitoring system with a
dimensional data model, flexible query language, efficient time series database
and modern alerting approach.")
    (home-page "https://github.com/prometheus/node_exporter")
    (license license:asl2.0)))
