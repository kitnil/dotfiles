(define-module (packages monitoring)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages monitoring)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
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

(define-public prometheus-tp-link-exporter
  (package
    (name "prometheus-tp-link-exporter")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/wigust/prometheus-tp-link-exporter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bb0kmk5yfzjr49pcdi7bsxj3zv53xkm4ghcwafswkn5ac8lwkbj"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (propagated-inputs
     (list jc python-prometheus-client))
    (home-page "https://gitlab.com/wigust/prometheus-tp-link-exporter")
    (synopsis "Prometheus TP-Link Exporter")
    (description
     "This package provides Prometheus TP-Link Exporter.")
    (license license:gpl3+)))

(define-public grafana
  (package
    (name "grafana")
    (version "8.3.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://dl.grafana.com/enterprise/release/grafana-enterprise-"
         version ".linux-amd64.tar.gz"))
       (sha256
        (base32
         "0fap4qp7dgwlhipj2djmi91wrydx1rjf62w70z22lgh8l4ngwkg8"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
    (inputs
     `(("glibc" ,glibc)
       ("patchelf" ,patchelf)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/bin"))
         (setenv "PATH" (string-append
                         (assoc-ref %build-inputs "tar") "/bin" ":"
                         (assoc-ref %build-inputs "gzip") "/bin" ":"
                         (assoc-ref %build-inputs "patchelf") "/bin"))
         (invoke "tar" "--strip-components=1"
                 "-xf" (assoc-ref %build-inputs "source")
                 "-C" %output)
         (chmod (string-append %output "/bin/grafana-server") #o755)
         (invoke "patchelf" "--set-interpreter"
                 (string-append (assoc-ref %build-inputs "glibc")
                                "/lib/ld-linux-x86-64.so.2")
                 (string-append %output "/bin/grafana-server")))))
    (home-page "https://grafana.com/")
    (synopsis "Platform for monitoring and observability")
    (description "Grafana allows you to query, visualize, alert on and
understand your metrics no matter where they are stored.  Create, explore, and
share dashboards with your team and foster a data driven culture:

@itemize
@item Visualize: Fast and flexible client side graphs with a multitude of
options.  Panel plugins offer many different ways to visualize metrics and
logs.
@item Dynamic Dashboards: Create dynamic & reusable dashboards with template
variables that appear as dropdowns at the top of the dashboard.
@item Explore Metrics: Explore your data through ad-hoc queries and dynamic
drilldown.  Split view and compare different time ranges, queries and data
sources side by side.
@item Explore Logs: Experience the magic of switching from metrics to logs
with preserved label filters.  Quickly search through all your logs or
streaming them live.
@item Alerting: Visually define alert rules for your most important metrics.
Grafana will continuously evaluate and send notifications to systems like
Slack, PagerDuty, VictorOps, OpsGenie.
@item Mixed Data Sources: Mix different data sources in the same graph! You
can specify a data source on a per-query basis.  This works for even custom
datasources.
@end itemize")
    (license license:agpl3)))

(define-public prometheus-lvm-exporter
  (package
    (name "prometheus-lvm-exporter")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/mjuh/monitoring-prometheus-lvm-exporter/releases/download/v"
         version "/prometheus-lvm-exporter"))
       (sha256
        (base32
         "0zj6nz9xlz21xbw5pyds1gk8yjh0acxwd2k739qw71gxl7kl6rkg"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          ,(string-append "/bin/prometheus-lvm-exporter")))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'chmod-binary
                    (lambda* (#:key outputs #:allow-other-keys)
                      (chmod (string-append (assoc-ref outputs "out")
                                            "/bin/prometheus-lvm-exporter")
                             #o555))))))
    (home-page "https://github.com/mjuh/monitoring-prometheus-lvm-exporter")
    (synopsis "LVM thin pool Prometheus exporter")
    (description "This program gets information about LVM thin pools with lvs
binary and provides LV's size and LV's free space as a Prometheus metrics.")
    (license license:asl2.0)))
