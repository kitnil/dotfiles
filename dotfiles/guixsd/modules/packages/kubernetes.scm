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

(define-public virtctl
  (package
    (name "virtctl")
    (version "0.53.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/kubevirt/kubevirt/releases/download/v"
                       version "/virtctl-v" version "-linux-amd64"))
       (sha256
        (base32
         "0b8aw3cywrqqq0nhmr4fiwvxb54dpga2h6yj8a4n46anckcffkvb"))))
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

(define-public k3d
  (package
    (name "k3d")
    (version "5.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/k3d-io/k3d/releases/download/v"
                           version "/k3d-linux-amd64"))
       (sha256
        (base32
         "1gd3693i1jx1l5rqa20yv6hv1chv4j9iapfr9nyi3rbrads52qbq"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bin (string-append #$output "/bin"))
                 (k3d (string-append bin "/k3d")))
            (mkdir-p bin)
            (copy-file #$(this-package-native-input "source") k3d)
            (chmod k3d #o555)))))
    (home-page "https://k3d.io/")
    (synopsis " Little helper to run CNCF's k3s in Docker")
    (description "k3d is a lightweight wrapper to run k3s (Rancher Lab’s minimal Kubernetes
distribution) in docker.")
    (license license:expat)))

(define-public kubectl
  (package
    (name "kubectl")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.k8s.io/release/v"
                                  version "/bin/linux/amd64/kubectl"))
              (sha256
               (base32
                "1mwqpghkrb64pmx45cnzmiwnnh5b13wyyax3wdczpxkjcyxqdmll"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bin (string-append #$output "/bin"))
                 (kubectl (string-append bin "/kubectl")))
            (mkdir-p bin)
            (copy-file #$(this-package-native-input "source") kubectl)
            (chmod kubectl #o555)))))
    (home-page "https://kubernetes.io/docs/tasks/tools/install-kubectl-linux/")
    (synopsis "Kubernetes CLI")
    (description "This package provides a Kubernetes CLI utility.")
    (license license:asl2.0)))

(define-public kompose
  (package
    (name "kompose")
    (version "1.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/kubernetes/kompose/releases/download/v"
                                  version "/kompose-linux-amd64"))
              (sha256
               (base32
                "0vx3af2gy6yqks8rj5b716g04ml5bjqi2jfykdg9hqyw0p1bi1fd"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bin (string-append #$output "/bin"))
                 (kompose (string-append bin "/kompose")))
            (mkdir-p bin)
            (copy-file #$(this-package-native-input "source") kompose)
            (chmod kompose #o555)))))
    (home-page "https://kubernetes.io/docs/tasks/tools/install-kompose-linux/")
    (synopsis "Kubernetes CLI")
    (description "This package provides a Kubernetes CLI utility.")
    (license license:asl2.0)))
