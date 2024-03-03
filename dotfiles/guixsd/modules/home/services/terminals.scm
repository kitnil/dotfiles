(define-module (home services terminals)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-alacritty-service
            home-screen-service
            home-qterminal-service
            home-kitty-service))

(define home-alacritty-service
  (simple-service 'alacritty-config
                  home-files-service-type
                  (list `(".config/alacritty/themes/xterm.toml" ,(local-file (string-append %project-directory "/dot_config/alacritty/themes/xterm.toml")))
                        `(".config/alacritty/alacritty.toml" ,(local-file (string-append %project-directory "/dot_config/alacritty/alacritty.toml"))))))

(define home-screen-service
  (simple-service 'screen-config
                  home-files-service-type
                  (list `(".screenrc" ,(local-file (string-append %project-directory "/dot_screenrc"))))))

(define home-qterminal-service
  (simple-service 'qterminal-config
                  home-files-service-type
                  (list `(".config/qterminal.org/qterminal.ini" ,(local-file (string-append %project-directory "/dot_config/qterminal.org/qterminal.ini")))
                        `(".config/qterminal.org/qterminal_bookmarks.xml" ,(local-file (string-append %project-directory "/dot_config/qterminal.org/qterminal_bookmarks.xml"))))))

(define home-kitty-service
  (simple-service 'kitty-config
                  home-files-service-type
                  (list `(".config/kitty/kitty.conf" ,(local-file (string-append %project-directory "/dot_config/kitty/kitty.conf"))))))
