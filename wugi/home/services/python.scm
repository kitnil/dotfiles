(define-module (wugi home services python)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (wugi home config)
  #:use-module (wugi utils)
  #:export (home-python-service))

(define home-python-service
  (simple-service 'python-config
                  home-files-service-type
                  (list `(".pythonrc" ,(local-file (string-append %distro-directory "/dot_pythonrc"))))))
