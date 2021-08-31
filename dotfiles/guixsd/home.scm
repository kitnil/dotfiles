(use-modules (gnu home)
	     (gnu home-services)
	     ;; (gnu home-services ssh)
	     (gnu home-services shells)
	     ;; (gnu home-services files)
	     (gnu services)
	     (gnu packages admin)
             (guix gexp)

             (ice-9 rdelim))

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

   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bashrc
              (list
               (with-input-from-file .bashrc read-string)))
             (bash-profile
              (list
               (with-input-from-file .bash_profile read-string)))))

   ;; XXX: missing home-files-service-type
   ;; (simple-service 'test-config
   ;;                 home-files-service-type
   ;;                 (list `("config/test.conf"
   ;;                         ,(plain-file "tmp-file.txt"
   ;;                                      "the content of ~/.config/test.conf"))))
   
   ;; XXX: missing home-ssh-configuration
   ;; (service home-ssh-service-type
   ;;          (home-ssh-configuration
   ;;           (extra-config
   ;;            (list
   ;;             (ssh-host "savannah"
   ;;      		 '((compression . #f)))))))

   )))
