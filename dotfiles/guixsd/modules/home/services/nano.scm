(define-module (home services nano)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-nano-service))

(define home-nano-service
  (simple-service 'nano-config
                  home-files-service-type
                  (list `(".nanorc" ,(local-file (string-append %project-directory "/dot_nanorc"))))))
