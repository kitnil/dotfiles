(define-module (home services kubernetes)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (home config)
  #:use-module (guile pass)
  #:export (kubernetes-service-type))

(define-record-type* <kubernetes-cluster-configuration>
  kubernetes-cluster-configuration make-kubernetes-cluster-configuration
  kubernetes-cluster-configuration?
  (name kubernetes-cluster-configuration-name) ;string
  (config-file kubernetes-cluster-configuration-config-file) ;string
  (output-directory kubernetes-cluster-configuration-output-directory ;string
                    ))

(define (kubernetes-configuration->vc config)
  (program-file
   (string-append "kubernetes-"
                  (kubernetes-cluster-configuration-name config)
                  "-configuration-to-version-control")
   (with-imported-modules '((guix build utils)
                            (ice-9 rdelim)
                            (ice-9 popen))
     #~(begin
         (use-modules (ice-9 rdelim)
                      (ice-9 popen)
                      (guix build utils))
         (let* ((kubernetes-directory
                 #$(string-append "kubernetes/" (kubernetes-cluster-configuration-name config)))
                (kubernetes-cluster-name "mj-k8s-cluster0-lb")
                (%home (and=> (getenv "HOME")
                              (lambda (home)
                                home)))
                (directory (string-append #$(kubernetes-cluster-configuration-output-directory config) "/"
                                          kubernetes-directory)))
           (mkdir-p directory)
           (with-directory-excursion directory
             (invoke "git" "rm" "-rf" "."))
           (invoke "docker" "run"
                   "--network" "host"
                   "--rm"
                   "--volume" (string-append %home "/.kube:/.kube")
                   "--volume" (string-append directory ":/dump")
                   "docker-registry.intr/utils/kube-dump:master"
                   "dump-namespaces"
                   "-d" "/dump"
                   "--kube-config"
                   #$(string-append "/.kube/" (kubernetes-cluster-configuration-config-file config)))
           (with-directory-excursion #$(kubernetes-cluster-configuration-output-directory config)
             (invoke "git" "add" kubernetes-directory)
             (invoke "git" "commit" "--message=Update.")))))))

(define (kubernetes-mcron-jobs config)
  (list
   #~(job
      '(next-hour '(17))
      #$(kubernetes-configuration->vc
         (kubernetes-cluster-configuration
          (name "home")
          (config-file "config-home-k8s")
          (output-directory "/home/oleg/src/cgit.duckdns.org/wigust/state-kubernetes-home"))))))

(define kubernetes-service-type
  (service-type
   (name 'kubernetes)
   (extensions
    (list (service-extension home-mcron-service-type
                             kubernetes-mcron-jobs)))
   (description
    "Periodically run Kubernetes configuration dump.")
   (default-value '())))
