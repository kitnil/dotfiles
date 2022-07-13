(define-module (home services guile)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-guile-service))

(define home-guile-service
  (simple-service 'guile-config
                  home-files-service-type
                  (list `(".guile" ,(local-file (string-append %project-directory "/dot_guile"))))))
