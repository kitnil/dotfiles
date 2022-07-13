(define-module (home services video)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-mpv-service))

(define home-mpv-service
  (simple-service 'mpv-config
                  home-files-service-type
                  (list `(".config/mpv/input.conf" ,(local-file (string-append %project-directory "/dot_config/mpv/input.conf")))
                        `(".config/mpv/mpv.conf" ,(local-file (string-append %project-directory "/dot_config/mpv/mpv.conf"))))))
