(define-module (home services python)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-python-service))

(define home-python-service
  (simple-service 'python-config
                  home-files-service-type
                  (list `(".pythonrc" ,(local-file (string-append %project-directory "/dot_pythonrc"))))))
