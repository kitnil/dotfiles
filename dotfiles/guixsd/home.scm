(use-modules (gnu home)
             (gnu home-services)
             ;; (gnu home-services files)
             (gnu home-services shells)
             ;; (gnu home-services ssh)
             (gnu packages admin)
             (gnu packages guile)
             (gnu services)
             (guix gexp)
             (guix modules)
             (ice-9 rdelim)
             (json))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define .bash_profile
  (string-append %home "/.local/share/chezmoi/dot_bash_profile"))

(define .bashrc
  (string-append %home "/.local/share/chezmoi/dot_bashrc"))

(home-environment

 ;; (packages (list htop))

 (services
  (list

   ;; (service home-bash-service-type
   ;;          (home-bash-configuration
   ;;           (guix-defaults? #t)
   ;;           (bashrc
   ;;            (list
   ;;             (with-input-from-file .bashrc read-string)))
   ;;           (bash-profile
   ;;            (list
   ;;             (with-input-from-file .bash_profile read-string)))))

   (simple-service 'amtool-config
                   home-files-service-type
                   (list `("config/amtool/config.yml"
                           ,(computed-file
                             "amtool-config.json"
                             (with-extensions (list guile-json-4)
                               (with-imported-modules (source-module-closure '((json builder)))
                                 #~(begin
                                     (use-modules (json builder))
                                     (with-output-to-file #$output
                                       (lambda ()
                                         (scm->json
                                          '(("output" . "extended")
                                            ;; ("receiver" . "team-X-pager")
                                            ;; ("comment_required" . #t)
                                            ;; ("author" . "user@example.org")
                                            ("alertmanager.url" . "http://localhost:9093"))
                                          #:pretty #t))))))))))
   
   ;; XXX: missing home-ssh-configuration
   ;; (service home-ssh-service-type
   ;;          (home-ssh-configuration
   ;;           (extra-config
   ;;            (list
   ;;             (ssh-host "savannah"
   ;;      		 '((compression . #f)))))))

   )))
