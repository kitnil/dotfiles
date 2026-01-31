(define-module (wugi home services mime)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (wugi home config)
  #:use-module (wugi utils)
  #:export (home-mime-service))


;;;
;;; Bash
;;;

(define home-mime-service
  (simple-service 'applications
                  home-files-service-type
                  (list `(".local/share/applications/mupdf.desktop" ,(local-file (string-append %distro-directory "/dot_local/share/applications/mupdf.desktop")))
                        `(".local/share/applications/org-protocol.desktop" ,(local-file (string-append %distro-directory "/dot_local/share/applications/org-protocol.desktop")))
                        `(".local/share/applications/mimeapps.list" ,(local-file (string-append %distro-directory "/dot_local/share/applications/mimeapps.list")))
                        `(".local/share/applications/guix-log.desktop" ,(local-file (string-append %distro-directory "/dot_local/share/applications/guix-log.desktop")))
                        `(".local/share/applications/feh.desktop" ,(local-file (string-append %distro-directory "/dot_local/share/applications/feh.desktop"))))))
