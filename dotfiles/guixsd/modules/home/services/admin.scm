(define-module (home services admin)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-htop-service))

(define home-htop-service
  (simple-service 'htop-config
                  home-files-service-type
                  (list `(".config/htop/htoprc" ,(local-file (string-append %project-directory "/dot_config/htop/htoprc"))))))
