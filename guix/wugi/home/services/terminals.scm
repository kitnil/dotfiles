(define-module (wugi home services terminals)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages terminals)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (wugi home config)
  #:use-module (wugi utils)
  #:use-module (wugi utils package)
  #:export (home-alacritty-service-type
            home-screen-service
            home-qterminal-service
            home-kitty-service))

(define alacritty-theme-xterm-light
  (plain-file "xterm.toml" "\
[colors.primary]
foreground = \"0x000000\"
background = \"0xFFFFFF\"

[colors.normal]
black = \"0x000000\"
red = \"0xB21818\"
green = \"0x66CD00\"
yellow = \"0xB26818\"
blue = \"0x4169e1\"
magenta = \"0xB218B2\"
cyan = \"0x18B2B2\"
white = \"0xB2B2B2\"

[colors.bright]
black = \"0x686868\"
red = \"0xFF5454\"
green = \"0x54FF54\"
yellow = \"0xEEC900\"
blue = \"0x5454FF\"
magenta = \"0xFF54FF\"
cyan = \"0x54FFFF\"
white = \"0xFFFFFF\"
"))

(define alacritty-xterm-light-config-file
  (mixed-text-file "alacritty.toml" "\
[env]
TERM = \"xterm-256color\"

[font]
size = 10
normal.family = \"Adwaita Mono\"
bold.family = \"Adwaita Mono\"
italic.family = \"Adwaita Mono\"
bold_italic.family = \"Adwaita Mono\"

[general]
import = [ \"" alacritty-theme-xterm-light "\" ]
live_config_reload = false"))

(define alacritty-xterm-light-program
  (program-file "alacritty-xterm-light"
                #~(let ((args (cdr (command-line)))
                        (alacritty-binary #$(file-append alacritty "/bin/alacritty")))
                    (apply execl
                           (append (list alacritty-binary
                                         alacritty-binary
                                         "--config-file" #$alacritty-xterm-light-config-file)
                                   args)))))

(define alacritty-theme-xterm-dark
  (plain-file "xterm.toml" "\
[colors.primary]
foreground = \"0xFFFFFF\"
background = \"0x000000\"

[colors.normal]
black = \"0x000000\"
red = \"0xB21818\"
green = \"0x66CD00\"
yellow = \"0xB26818\"
blue = \"0x4169e1\"
magenta = \"0xB218B2\"
cyan = \"0x18B2B2\"
white = \"0xB2B2B2\"

[colors.bright]
black = \"0x686868\"
red = \"0xFF5454\"
green = \"0x54FF54\"
yellow = \"0xEEC900\"
blue = \"0x5454FF\"
magenta = \"0xFF54FF\"
cyan = \"0x54FFFF\"
white = \"0xFFFFFF\"
"))

(define alacritty-xterm-dark-config-file
  (mixed-text-file "alacritty.toml"
                   "\
[env]
TERM = \"xterm-256color\"

[font]
size = 10
normal.family = \"Adwaita Mono\"
bold.family = \"Adwaita Mono\"
italic.family = \"Adwaita Mono\"
bold_italic.family = \"Adwaita Mono\"

[general]
import = [ \"" alacritty-theme-xterm-dark "\" ]
live_config_reload = false"
"\n"))

(define alacritty-xterm-dark-program
  (program-file "alacritty-xterm-dark"
                #~(let ((args (cdr (command-line)))
                        (alacritty-binary #$(file-append alacritty "/bin/alacritty")))
                    (apply execl
                           (append (list alacritty-binary
                                         alacritty-binary
                                         "--config-file" #$alacritty-xterm-dark-config-file)
                                   args)))))

(define home-alacritty-service-type
  (service-type
   (name 'alacritty)
   (extensions
    (list
     (service-extension home-profile-service-type
                        (lambda (config)
                          (list alacritty
                                (package-from-program-file
                                 alacritty-xterm-light-program)
                                (package-from-program-file
                                 alacritty-xterm-dark-program))))
     (service-extension home-files-service-type
                        (lambda (config)
                          (list `(".config/alacritty/alacritty.toml"
                                  ,alacritty-xterm-dark-config-file))))))
   (default-value '())
   (description "Configure Alacritty, terminal emulator.")))

(define home-screen-service
  (simple-service 'screen-config
                  home-files-service-type
                  (list `(".screenrc" ,(local-file (string-append %distro-directory "/dot_screenrc"))))))

(define home-qterminal-service
  (simple-service 'qterminal-config
                  home-files-service-type
                  (list `(".config/qterminal.org/qterminal.ini" ,(local-file (string-append %distro-directory "/dot_config/qterminal.org/qterminal.ini")))
                        `(".config/qterminal.org/qterminal_bookmarks.xml" ,(local-file (string-append %distro-directory "/dot_config/qterminal.org/qterminal_bookmarks.xml"))))))

(define home-kitty-service
  (simple-service 'kitty-config
                  home-files-service-type
                  (list `(".config/kitty/kitty.conf" ,(local-file (string-append %distro-directory "/dot_config/kitty/kitty.conf"))))))
