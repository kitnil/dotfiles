(define-module (home services rust-apps)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-ripgrep-service))

(define home-ripgrep-service
  (simple-service 'ripgrep-config
                  home-files-service-type
                  (list `(".config/ripgrep/ripgreprc" ,(local-file (string-append %project-directory "/dot_config/ripgrep/ripgreprc"))))))
