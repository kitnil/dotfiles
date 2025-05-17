(define-module (wugi home services lisp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (wugi home config)
  #:use-module (wugi utils)
  #:export (home-sbcl-service))

(define home-sbcl-service
  (simple-service 'sbcl-config
                  home-files-service-type
                  (list `(".sbcl_completions" ,(local-file (string-append %distro-directory "/dot_sbcl_completions"))))))
