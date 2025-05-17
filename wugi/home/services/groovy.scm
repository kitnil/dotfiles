(define-module (wugi home services groovy)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (wugi home config)
  #:use-module (wugi utils)
  #:export (home-groovy-service))

(define home-groovy-service
  (simple-service 'groovy-config
                  home-files-service-type
                  (list `(".groovy/groovysh.rc" ,(local-file (string-append %distro-directory "/dot_groovy/groovysh.rc"))))))
