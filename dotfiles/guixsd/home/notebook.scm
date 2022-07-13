(use-modules (gnu home)
             (gnu home services)
             (gnu home services mcron)
             (gnu home services shells)
             (gnu home services ssh)
	     (gnu services)
	     (guix gexp))

(define %project-directory
  (string-append (dirname (current-filename)) "/../../.."))

(home-environment
 (services
  (list
   (simple-service 'sway-config
                   home-files-service-type
                   (list `(".config/sway/config" ,(local-file (string-append %project-directory "/dot_config/sway/notebook.config")))
			 `(".config/sway/status.sh" ,(local-file (string-append %project-directory "/dot_config/sway/status.sh") #:recursive? #t)))))))
