(define-module (home services web)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-chromium-service
            home-youtube-dl-service))

(define home-chromium-service
  (simple-service 'chromium-wrapper
                  home-files-service-type
                  (map (lambda (program)
                         `(,(string-append ".local/bin/" program)
                           ,(computed-file
                             program
                             #~(begin
                                 (with-output-to-file #$output
                                   (lambda ()
                                     (format #t "\
#!/bin/sh
# https://github.com/stumpwm/stumpwm/issues/894
export FONTCONFIG_FILE=/run/current-system/profile/etc/fonts/fonts.conf
exec -a \"$0\" /home/oleg/.nix-profile/bin/~a --disable-features=SendMouseLeaveEvents \"$@\"\n"
                                             #$program)))
                                 (chmod #$output #o555)))))
                       '("google-chrome-stable" "chromium"))))

(define home-youtube-dl-service
  (simple-service 'youtube-dl-config
                  home-files-service-type
                  (list `(".config/youtube-dl/config" ,(local-file (string-append %project-directory "/dot_config/youtube-dl/config"))))))
