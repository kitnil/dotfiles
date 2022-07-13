(define-module (home services gnupg)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-gnupg-service))

(define home-gnupg-service
  (simple-service 'gnupg-config
                  home-files-service-type
                  (map (lambda (file-name)
                         `(,(string-append ".gnupg/" file-name) ,(local-file (string-append %project-directory "/private_dot_gnupg/" file-name))))
                       '("gpg-agent.conf"
                         "gpg.conf"))))
