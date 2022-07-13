(define-module (home services databases)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-mongo-service
            home-mycli-service
            home-postgresql-service))

(define home-mongo-service
  (simple-service 'mongo-config
                  home-files-service-type
                  (list `(".mongorc.js" ,(local-file (string-append %project-directory "/dot_mongorc.js"))))))

(define home-mycli-service
  (simple-service 'mycli-config
                  home-files-service-type
                  (list `(".myclirc" ,(local-file (string-append %project-directory "/dot_myclirc"))))))

(define home-postgresql-service
  (simple-service 'postgresql-config
                  home-files-service-type
                  (list `(".config/autopostgresqlbackup.conf" ,(local-file (string-append %project-directory "/dot_config/autopostgresqlbackup.conf"))))))
