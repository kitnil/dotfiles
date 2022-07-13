(define-module (home services gtk)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-gtk-service
            home-gtkrc-service))

(define home-gtk-service
  (simple-service 'gtk-config
                  home-files-service-type
                  (list `(".config/gtk-3.0/gtk.css" ,(local-file (string-append %project-directory "/dot_config/gtk-3.0/gtk.css")))
                        `(".config/gtk-3.0/settings.ini" ,(local-file (string-append %project-directory "/dot_config/gtk-3.0/settings.ini"))))))

(define home-gtkrc-service
  (simple-service 'gtkrc-config
                  home-activation-service-type
                  #~(begin
                      (let ((%home (and=> (getenv "HOME")
                                          (lambda (home)
                                            home))))
                        (call-with-output-file (string-append %home "/.gtkrc-2.0")
                          (lambda (port)
                            (format port "\
# DO NOT EDIT! This file will be overwritten by LXAppearance.
# Any customization should be done in ~/.gtkrc-2.0.mine instead.

include \"~a/.gtkrc-2.0.mine\"
gtk-theme-name=\"Adwaita-dark\"
gtk-icon-theme-name=\"Adwaita\"
gtk-font-name=\"DejaVu Sans 11\"
gtk-cursor-theme-name=\"Adwaita\"
gtk-cursor-theme-size=0
gtk-toolbar-style=GTK_TOOLBAR_BOTH
gtk-toolbar-icon-size=GTK_ICON_SIZE_SMALL_TOOLBAR
gtk-button-images=1
gtk-menu-images=1
gtk-enable-event-sounds=1
gtk-enable-input-feedback-sounds=1
gtk-xft-antialias=1
gtk-xft-hinting=1
gtk-xft-hintstyle=\"hintfull\"
gtk-xft-rgba=\"rgb\"
"
                                    %home)))))))
