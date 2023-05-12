(define-module (packages kubernetes)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rsync)
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
    (version "1.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.k8s.io/release/v"
                                  version "/bin/linux/amd64/kubectl"))
              (sha256
               (base32
                "0pbfgkbg5xih73lnrirnif7qfqypaf4gr32kzdy45xr5z8nf4yym"))))
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

(define-public kubernetes-helm
  (package
    (name "kubernetes-helm")
    (version "3.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://get.helm.sh/helm-v"
                                  version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "0c85qbj16fwzadkl7zcw1l6xjvv5nsrh1kqc8iq9d0m1d20f6nrz"))))
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
          (use-modules (guix build utils))
          (let ((bin (string-append #$output "/bin")))
            (mkdir-p (string-append %output "/bin"))
            (setenv "PATH" (string-append
                            (assoc-ref %build-inputs "tar") "/bin" ":"
                            (assoc-ref %build-inputs "gzip") "/bin"))
            (invoke "tar" "--strip-components=1"
                    "-xf" (assoc-ref %build-inputs "source")
                    "-C" (string-append %output "/bin"))))))
    (home-page "https://helm.sh/")
    (synopsis "Package manager for kubernetes")
    (description "This package provides a package manager for kubernetes.")
    (license license:asl2.0)))

(define-public crictl
  (package
    (name "crictl")
    (version "1.24.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/kubernetes-sigs/cri-tools.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "077zl2l7dplwb13a004p3lp5dhf0scpzfg4b4n12na5y701kkrln"))))
    (build-system go-build-system)
    (arguments
     `(;#:import-path "github.com/kubernetes-sigs/cri-tools"
       #:install-source? #f
       #:tests? #f ; tests require 'framwork' from kubernetes
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "src") #t))
         (add-before 'build 'prepare-source
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("/usr/local") out)
                 (("^VERSION .*") (string-append "VERSION := " ,version "\n")))
               #t)))
         (replace 'build
           (lambda _
             (invoke "make")))
         ;(replace 'check
         ;  (lambda _
         ;    (invoke "make" "test-e2e")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "BINDIR" (string-append out "/bin"))
               (invoke "make" "install"))))
         (delete 'install-license-files))))
    (home-page "https://github.com/kubernetes-sigs/cri-tools")
    (synopsis "CLI and validation tools for Kubelet Container Runtime Interface")
    (description "Cri-tools aims to provide a series of debugging and validation
tools for Kubelet CRI.")
    (license license:asl2.0)))

(define-public kubernetes
  (package
    (name "kubernetes")
    (version "1.24.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/kubernetes/kubernetes.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04lqvhg9xjbg6gxy5fym636nk2941xmz2im8x2wlbx598dn8r0xm"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "k8s.io/kubernetes"
       #:install-source? #f
       #:tests? #f ; Skip tests for now.
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-build
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/k8s.io/kubernetes"
               (substitute* '("build/root/Makefile"
                              "build/root/Makefile.generated_files"
                              "build/pause/Makefile")
                 (("/bin/bash") (which "bash")))
               (substitute* "pkg/util/mount/mount.go"
                 (("defaultMountCommand.*")
                  (string-append "defaultMountCommand = \""
                                 (assoc-ref inputs "util-linux")
                                 "/bin/mount\"\n"))))
             #t))
         (add-before 'build 'fix-version-numbers
           (lambda _
             (with-directory-excursion "src/k8s.io/kubernetes"
               (substitute* '("cmd/kubeadm/app/version/base.go"
                              "staging/src/k8s.io/client-go/pkg/version/base.go"
                              "staging/src/k8s.io/kubectl/pkg/version/base.go"
                              "staging/src/k8s.io/component-base/version/base.go"
                              "staging/src/k8s.io/component-base/metrics/version_parser_test.go"
                              "pkg/version/base.go"
                              "vendor/k8s.io/client-go/pkg/version/base.go"
                              "vendor/k8s.io/kubectl/pkg/version/base.go"
                              "vendor/k8s.io/component-base/metrics/version_parser_test.go")
                 (("v0.0.0-master\\+\\$Format:\\%h\\$") (string-append "v" ,version))
                 (("v0.0.0-master") (string-append "v" ,version))
                 (("gitMajor string = \"\"")
                  (string-append "gitMajor string = \"" ,(version-major version) "\""))
                 (("gitMinor string = \"\"")
                  (string-append "gitMinor string = \""
                                 ,(string-drop (version-major+minor version) 2) "\""))))
             #t))
         (replace 'build
           (lambda _
             (with-directory-excursion "src/k8s.io/kubernetes"
               ;; Cannot find go-bindata otherwise.
               (setenv "PATH" (string-append (getcwd) "/_output/bin:"
                                             (getenv "PATH")))
               (invoke "make"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "src/k8s.io/kubernetes"
                 ;; This saves more than 350MiB.
                 (delete-file "_output/local/go/bin/e2e.test")
                 (delete-file "_output/local/go/bin/e2e_node.test")
                 (for-each
                   (lambda (file)
                     (install-file file (string-append out "/bin")))
                   (find-files "_output/local/go/bin" ".*"))
                 ;(mkdir-p (string-append out "/share/bash-completion/completions"))
                 ;(call-with-output-file (string-append out "/share/bash-completion/completions/kubectl")
                 ;  (lambda (port)
                 ;    (format port (invoke "_output/local/go/bin/kubectl" "completion" "bash"))))
                 ;(mkdir-p (string-append out "/share/zsh/site-function"))
                 ;(call-with-output-file (string-append out "/share/zsh/site-functions/_kubectl")
                 ;  (lambda (port)
                 ;    (format port (invoke "_output/local/go/bin/kubectl" "completion" "zsh"))))
                 (install-file "LICENSE"
                               (string-append out "/share/doc/"
                                              ,name "-" ,version)))
               #t)))
         (add-after 'install 'install-man-pages
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/man/man1"))
               (with-directory-excursion "src/k8s.io/kubernetes"
                 (for-each
                   (lambda (file)
                     (invoke "_output/local/go/bin/genman"
                             (string-append out "/share/man/man1") file))
                   '("kube-apiserver" "kube-controller-manager" "kube-proxy"
                     "kube-scheduler" "kubelet" "kubectl")))
               #t))))))
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("rsync" ,rsync)
       ("util-linux" ,util-linux)))
    (propagated-inputs
     `(("crictl" ,crictl))) ; Must be the same major+minor version as kubernetes.
    (home-page "https://kubernetes.io/")
    (synopsis "Production-Grade Container Scheduling and Management")
    (description "Kubernetes is an open source system for managing containerized
applications across multiple hosts.  It provides basic mechanisms for
deployment, maintenance, and scaling of applications.")
    (license license:asl2.0)))

(define-public kubernetes-binary
  (package
    (name "kubernetes-binary")
    (version "1.24.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.k8s.io/v"
                           version "/kubernetes-server-linux-amd64.tar.gz"))
       (sha256
        (base32
         "1589pb8zq5pglrbhavymhhlgwn5n81sdrxdw2hi0b3k94afnwqmj"))))
    (build-system trivial-build-system)
    (native-inputs (list source gzip tar))
    (inputs (list glibc patchelf))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((bin (string-append #$output "/bin")))
            (mkdir-p (string-append #$output "/bin"))
            (setenv "PATH" (string-append
                            #$(this-package-native-input "tar") "/bin" ":"
                            #$(this-package-native-input "gzip") "/bin" ":"
                            #$(this-package-input "patchelf") "/bin"))
            (invoke "tar" "--strip-components=2"
                    "-xf" (assoc-ref %build-inputs "source"))
            ;; dynamically linked
            (for-each (lambda (file)
                        (invoke "patchelf" "--set-interpreter"
                                (string-append #$(this-package-input "glibc")
                                               "/lib/ld-linux-x86-64.so.2")
                                (string-append "bin/" file))
                        (install-file (string-append "bin/" file) bin))
                      '("apiextensions-apiserver"
                        "kube-aggregator"
                        "kubectl-convert"
                        "kubelet"))
            ;; statically linked
            (for-each (lambda (file)
                        (install-file (string-append "bin/" file) bin))
                      '("mounter"
                        "kube-apiserver"
                        "kube-controller-manager"
                        "kube-proxy"
                        "kubectl"
                        "kube-log-runner"
                        "kubeadm"
                        "kube-scheduler"))))))
    (home-page "https://kubernetes.io/")
    (synopsis "Production-Grade Container Scheduling and Management")
    (description "Kubernetes is an open source system for managing containerized
applications across multiple hosts.  It provides basic mechanisms for
deployment, maintenance, and scaling of applications.")
    (license license:asl2.0)))

(define-public etcd
  (package
    (name "etcd")
    (version "3.4.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/etcd-io/etcd/releases/download/v"
                           version "/etcd-v" version "-linux-amd64.tar.gz"))
       (sha256
        (base32
         "1kcwrng9h0rmkp85d8bwl8ghgn49451l14787mnsy7vsq4khx9wv"))))
    (build-system trivial-build-system)
    (native-inputs (list source gzip tar))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((bin (string-append #$output "/bin")))
            (mkdir-p (string-append #$output "/bin"))
            (setenv "PATH" (string-append
                            #$(this-package-native-input "tar") "/bin" ":"
                            #$(this-package-native-input "gzip") "/bin"))
            (invoke "tar" "--strip-components=1"
                    "-xf" (assoc-ref %build-inputs "source")
                    "-C" bin)))))
    (home-page "https://github.com/etcd-io/etcd")
    (synopsis "Distributed reliable key-value store")
    (description "etcd is a distributed reliable key-value store for the most critical data of
a distributed system, with a focus on being:

@item Simple: well-defined, user-facing API (gRPC)
@item Secure: automatic TLS with optional client cert authentication
@item Fast: benchmarked 10,000 writes/sec
@item Reliable: properly distributed using Raft\n")
    (license license:asl2.0)))

(define-public k9s
  (package
    (name "k9s")
    (version "0.26.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/derailed/k9s/releases/download/v"
                           version "/k9s_Linux_x86_64.tar.gz"))
       (sha256
        (base32
         "1kv2q5gvh4d584r5fyvwqc5d9wpp8cfh5jxzn8dfjvx4rwbsqirl"))))
    (build-system trivial-build-system)
    (native-inputs (list source gzip tar))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((bin (string-append #$output "/bin")))
            (mkdir-p (string-append #$output "/bin"))
            (setenv "PATH" (string-append
                            #$(this-package-native-input "tar") "/bin" ":"
                            #$(this-package-native-input "gzip") "/bin"))
            (invoke "tar"
                    "-xf" (assoc-ref %build-inputs "source")
                    "-C" bin
                    "k9s")))))
    (home-page "https://k9scli.io/")
    (synopsis "Kubernetes CLI to manage Kubernetes clusters")
    (description "K9s provides a terminal UI to interact with your Kubernetes clusters. The aim
of this project is to make it easier to navigate, observe and manage your
applications in the wild. K9s continually watches Kubernetes for changes and
offers subsequent commands to interact with your observed resources.")
    (license license:asl2.0)))

(define-public argocd
  (package
    (name "argocd")
    (version "2.4.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/argoproj/argo-cd/releases/download/v"
                                  version "/argocd-linux-amd64"))
              (sha256
               (base32
                "0504j5xcz1d6w863qb0z968a0k5nj6ls4l2kvcmbk3zvsjv1frvc"))))
    (build-system trivial-build-system)
    (native-inputs (list source))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bin (string-append #$output "/bin"))
                 (argocd (string-append bin "/argocd")))
            (mkdir-p (string-append #$output "/bin"))
            (copy-file (assoc-ref %build-inputs "source")
                       (string-append #$output "/bin/argocd"))
            (chmod argocd #o555)))))
    (home-page "https://argo-cd.readthedocs.io/")
    (synopsis "Declarative continuous deployment for Kubernetes")
    (description "Argo CD is a declarative, GitOps continuous delivery tool
 for Kubernetes.")
    (license license:asl2.0)))
