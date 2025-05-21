(define-module (wugi home services kodi)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (wugi home config)
  #:use-module (wugi utils)
  #:export (home-kodi-service))

(define home-kodi-service
  (simple-service 'kodi-config
                  home-files-service-type
                  (list `(".kodirc" ,(local-file (string-append %distro-directory "/dot_kodirc"))))))
