(define-module (home services groovy)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-groovy-service))

(define home-groovy-service
  (simple-service 'groovy-config
                  home-files-service-type
                  (list `(".groovy/groovysh.rc" ,(local-file (string-append %project-directory "/dot_groovy/groovysh.rc"))))))
