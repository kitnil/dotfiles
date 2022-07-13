(define-module (home services linux)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-top-service))

(define home-top-service
  (simple-service 'top-config
                  home-files-service-type
                  (list `(".toprc" ,(local-file (string-append %project-directory "/dot_toprc"))))))
