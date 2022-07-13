(define-module (home services version-control)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-git-service
            home-gita-service))

(define home-git-service
  (simple-service 'gitconfig-config
                  home-files-service-type
                  (list `(".gitconfig" ,(local-file (string-append %project-directory "/dot_gitconfig")))
                        `(".config/git/gitk" ,(local-file (string-append %project-directory "/dot_config/git/gitk")))
                        `(".config/git/ignore" ,(local-file (string-append %project-directory "/dot_config/git/ignore"))))))

(define home-gita-service
  (simple-service 'gita-config
                  home-files-service-type
                  (list `(".config/gita/cmds.yml" ,(local-file (string-append %project-directory "/dot_config/gita/cmds.yml"))))))
