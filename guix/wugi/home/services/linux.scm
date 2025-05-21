(define-module (wugi home services linux)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (wugi home config)
  #:use-module (wugi utils)
  #:export (home-top-service))

(define home-top-service
  (simple-service 'top-config
                  home-files-service-type
                  (list `(".toprc" ,(local-file (string-append %distro-directory "/dot_toprc"))))))
