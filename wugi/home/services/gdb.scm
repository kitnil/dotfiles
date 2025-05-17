(define-module (wugi home services gdb)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (wugi home config)
  #:use-module (wugi utils)
  #:export (home-gdb-service))

(define home-gdb-service
  (simple-service 'gdb-config
                  home-files-service-type
                  (list `(".gdbinit" ,(local-file (string-append %distro-directory "/dot_gdbinit"))))))
