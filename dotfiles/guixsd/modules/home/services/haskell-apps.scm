(define-module (home services haskell-apps)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:use-module (gnu packages haskell-apps)
  #:export (home-greenclip-service
            home-shellcheck-service
            home-ghci-service))

(define home-greenclip-service
  (simple-service 'greenclip-config
                  home-files-service-type
                  (list `(".config/greenclip.cfg" ,(local-file (string-append %project-directory "/dot_config/greenclip.cfg"))))))

(define home-shellcheck-service
  (simple-service 'shellcheck-wrapper
                  home-files-service-type
                  (list `(".local/bin/shellcheck"
                          ,(computed-file
                            "shellcheck-wrapper"
                            #~(begin
                                (with-output-to-file #$output
                                  (lambda ()
                                    (format #t "\
#!/bin/sh
exec -a \"$0\" ~a/bin/shellcheck --shell=bash \"$@\"\n"
                                            #$shellcheck)))
                                (chmod #$output #o555)))))))

(define home-ghci-service
  (simple-service 'ghci-config
                  home-files-service-type
                  (list `(".ghci" ,(local-file (string-append %project-directory "/dot_ghci"))))))
