(define-module (home services kubernetes)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (home config)
  #:use-module (guile pass)
  #:export (kubernetes-service-type))

(define (kubernetes-configuration->vc)
  (program-file
   "kubernetes-configuration-to-version-control"
   (with-imported-modules '((guix build utils)
                            (ice-9 rdelim)
                            (ice-9 popen))
     #~(begin
         (use-modules (ice-9 rdelim)
                      (ice-9 popen)
                      (guix build utils))
         (let* ((kubernetes-directory "kubernetes")
                (kubernetes-cluster-name "mj-k8s-cluster0-lb")
                (%home (and=> (getenv "HOME")
                              (lambda (home)
                                home)))
                (directory (string-append #$%ansible-state-directory "/"
                                          kubernetes-directory)))
           (mkdir-p directory)
           (let* ((port (open-pipe* OPEN_READ "kubectl"
                                    "config" "current-context"))
                  (output (read-string port)))
             (close-port port)
             (when (string= (string-trim-right output #\newline)
                            kubernetes-cluster-name)
               (invoke "docker" "run"
                       "--network" "host"
                       "--tty"
                       "--interactive"
                       "--rm"
                       "--volume" (string-append %home "/.kube-view:/.kube")
                       "--volume" (string-append directory ":/dump")
                       "woozymasta/kube-dump:latest"
                       "dump-namespaces"
                       "-d" "/dump"
                       "--kube-config" "/.kube/config")
               (with-directory-excursion #$%ansible-state-directory
                 (invoke "git" "add" kubernetes-directory)
                 (invoke "git" "commit" "--message=Update.")))))))))

(define (kubernetes-mcron-jobs config)
  (list
   #~(job
      '(next-hour '(15))
      #$(kubernetes-configuration->vc))))

(define kubernetes-service-type
  (service-type
   (name 'kubernetes)
   (extensions
    (list (service-extension home-mcron-service-type
                             kubernetes-mcron-jobs)))
   (description
    "Periodically run Kubernetes configuration dump.")
   (default-value '())))
