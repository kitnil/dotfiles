(define-module (wugi home services version-control)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (wugi home config)
  #:use-module (wugi utils)
  #:export (home-git-service
            home-gita-service
            home-gitconfig-service))

(define home-git-service
  (simple-service 'gitconfig2-config
                  home-files-service-type
                  (list `(".gitconfig2" ,(local-file (string-append %distro-directory "/dot_gitconfig")))
                        `(".config/git/gitk" ,(local-file (string-append %distro-directory "/dot_config/git/gitk")))
                        `(".config/git/ignore" ,(local-file (string-append %distro-directory "/dot_config/git/ignore"))))))

(define home-gitconfig-service
  (simple-service 'gitconfig-config
                  home-activation-service-type
                  (let ((file
                         (scheme-file
                          "generate-gitconfig-file.scm"
                          #~(begin
                              (let ((%home
                                     (and=> (getenv "HOME")
                                            (lambda (home)
                                              home))))
                                (use-modules (ice-9 format))
                                (define gitconfig-file
                                  (string-append %home "/.gitconfig"))
                                (call-with-output-file gitconfig-file
                                  (lambda (port)
                                    (unless (file-exists? gitconfig-file)
                                      (format port "\
[include]
        path = ~a/.gitconfig2
" home)))))))))
                    #~(begin (primitive-load #$file)))))

(define home-gita-service
  (simple-service 'gita-config
                  home-files-service-type
                  (list `(".config/gita/cmds.yml" ,(local-file (string-append %distro-directory "/dot_config/gita/cmds.yml"))))))
