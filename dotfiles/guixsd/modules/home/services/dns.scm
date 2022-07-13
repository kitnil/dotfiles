(define-module (home services dns)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-bind-utils-service))

(define home-bind-utils-service
  (simple-service 'dig-config
                  home-files-service-type
                  (list `(".digrc" ,(local-file (string-append %project-directory "/dot_digrc"))))))
