(define-module (wugi home services nano)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (wugi home config)
  #:use-module (wugi utils)
  #:export (home-nano-service))

(define home-nano-service
  (simple-service 'nano-config
                  home-files-service-type
                  (list `(".nanorc" ,(local-file (string-append %distro-directory "/dot_nanorc"))))))
