(use-modules (gnu home)
             (gnu home services)
             (gnu home services desktop)
             ;; (gnu home services files)
             (gnu home services mcron)
             (gnu home services shells)
             (gnu home services sound)
             (gnu home services ssh)
             (gnu packages admin)
             (gnu packages bash)
             (gnu packages guile)
             (gnu packages pulseaudio)
             (gnu packages virtualization)
             (gnu packages terminals)
             (gnu packages xdisorg)
             (gnu packages xorg)
             (gnu services)
             (gnu services configuration)
             (guix gexp)
             (guix modules)
             (guix profiles)
             (ice-9 rdelim)
             (json)

             (gnu packages base)
             (gnu packages haskell-apps)
             (gnu packages wm)

             (home config)
             (home config openssh)
             (home services ansible)
             (home services cisco)
             (home services desktop)
             (home services gdb)
             (home services emacs)
             (home services mail)
             (home services monitoring)
             (home services nix)
             (home services package-management)
             (home services shell)
             (home services version-control)
             (home services terminals)
             (home services tmux)
             (home services linux)
             (home services haskell-apps)
             (home services gtk)
             (home services stumpwm)
             (home services rust-apps)
             (home services lisp)
             (home services python)
             (home services nano)
             (home services dns)
             (home services web)
             (home services gnupg)
             (home services groovy)
             (home services guile)
             (home services kodi)
             (home services databases)
             (home services mime)
             (home services video)
             (home services networking)
             (home services kubernetes)
             (home services majordomo billing2)
             (home services audio)

             (gnu packages mail)
             (guile pass)

             ;; (dwl-guile home-service)
             ;; (dwl-guile configuration)
             )

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(add-to-load-path (string-append %home "/.local/bin"))
(use-modules (mjru-github-projects))

(define .bash_profile
  (string-append %home "/.local/share/chezmoi/dot_bash_profile"))

(define .bashrc
  (string-append %home "/.local/share/chezmoi/dot_bashrc"))

(define xmodmap-script
  (program-file
   "xmodmap"
   #~(begin
       (use-modules (srfi srfi-1)
                    (ice-9 popen)
                    (ice-9 rdelim))

       (define %home
         (and=> (getenv "HOME")
                (lambda (home)
                  home)))

       (define xmodmap
         #$(file-append xmodmap "/bin/xmodmap"))

       (define count (make-parameter 0))

       (let loop ()
         (let* ((port (open-pipe* OPEN_READ xmodmap "-pke"))
                (output (read-string port)))
           (close-pipe port)
           (if (or (< 2 (count))
                   (any (lambda (str)
                          (string= str "keycode 134 = Control_L NoSymbol Control_L"))
                        (string-split (string-trim-right output #\newline) #\newline)))
               #t
               (begin
                 (count (1+ (count)))
                 (system* xmodmap (string-append %home "/.Xmodmap"))
                 (sleep 3)
                 (loop))))))))

(define* (majordomo-mbsync-services name)
  (define (pass-private-or-public name)
    (if (file-exists? (string-append %home "/.password-store/majordomo/private/router.majordomo.ru/" name "@majordomo.ru.gpg"))
        (string-append "majordomo/private/router.majordomo.ru/"
                       name "@majordomo.ru")
        (string-append "majordomo/public/router.majordomo.ru/"
                       name "@majordomo.ru")))

  (define majordomo-mbsync-imap-account-configuration
    (mbsync-imap-account-configuration
     (host "router.majordomo.ru")
     (auth-mechs '("LOGIN"))
     (ssl-type "None")
     (certificate-file "/etc/ssl/certs/ca-certificates.crt")
     (pipeline-depth 50)))

  (define majordomo-mbsync-imap-store-configuration
    (mbsync-imap-store-configuration
     (imap-store "majordomo-remote")
     (account "majordomo")))

  (define majordomo-mbsync-maildir-store-configuration
    (mbsync-maildir-store-configuration
     (path "~/Maildir/")
     (sub-folders "Verbatim")))

  (define majordomo-mbsync-channel-configuration
    (mbsync-channel-configuration
     (patterns '("INBOX"))
     (sync '("Pull"))
     (expunge "None")))

  (list
   (simple-service (symbol-append 'home-mbsync-majordomo-
                                  (string->symbol name))
                   home-mbsync-service-type
                   (mbsync-configuration
                    (imap-accounts
                     (list
                      (mbsync-imap-account-configuration
                       (inherit majordomo-mbsync-imap-account-configuration)
                       (imap-account (string-append "majordomo-" name))
                       (user (string-append name "@majordomo.ru"))
                       (ssl-type "IMAPS")
                       (pass-cmd
                        (string-join
                         (list "pass" "show"
                               (pass-private-or-public name)))))))
                    (imap-stores
                     (list
                      (mbsync-imap-store-configuration
                       (imap-store (string-append "majordomo-" name "-remote"))
                       (account (string-append "majordomo-" name)))))
                    (maildir-stores
                     (list
                      (mbsync-maildir-store-configuration
                       (inherit majordomo-mbsync-maildir-store-configuration)
                       (maildir-store (string-append "majordomo-" name "-local"))
                       (inbox (string-append "~/Maildir/majordomo-" name)))))
                    (channels
                     (list
                      (mbsync-channel-configuration
                       (inherit majordomo-mbsync-channel-configuration)
                       (channel (string-append "majordomo-" name))
                       (far (string-append ":majordomo-" name "-remote:"))
                       (near (string-append ":majordomo-" name "-local:")))))))))

(define xsession-config-file
  (let* ((stumpwp-load-file
          (plain-file
           "stumpwp-load-file"
           (with-output-to-string
             (lambda ()
               (display '(require :asdf))
               (newline)
               (display '(require :stumpwm))
               (newline)
               (display '(stumpwm:stumpwm))
               (newline)))))
         (xsession-file
          (program-file
           "xsession"
           #~(begin
               (use-modules (srfi srfi-1)
                            (ice-9 popen)
                            (ice-9 rdelim)
                            (ice-9 format))

               (define %display
                 (and=> (getenv "DISPLAY")
                        (lambda (display)
                          display)))

               (define %home
                 (and=> (getenv "HOME")
                        (lambda (home)
                          home)))

               (display "Set background\n")
               (system* #$(file-append xsetroot "/bin/xsetroot")
                        "-solid" "black")

               (display "Set cursor theme\n")
               (system* #$(file-append xsetroot "/bin/xsetroot")
                        "-cursor_name" "left_ptr")

               (display "Disable speaker\n")
               (system* #$(file-append xset "/bin/xset") "-b")

               (display "Configure keymap\n")
               (system* #$xmodmap-script)

               (system* #$(file-append setxkbmap "/bin/setxkbmap")
                        "-layout" "us,ru" "-option" "grp:win_space_toggle")

               ;; Prepare environment for VNC sessions
               (display "Start window manager\n")
               (unsetenv "SESSION_MANAGER")
               (unsetenv "DBUS_SESSION_BUS_ADDRESS")
               (system* #$(file-append xhost "/bin/xhost") "+local:")
               (let* ((pw    (getpw (getuid)))
                      (shell (passwd:shell pw)))
                 (if (or (string= %display ":0.0")
                         (string= %display ":3.0"))
                     ;; The '--login' option is supported at least by Bash and zsh.
                     (execl shell "stumpwm" "--login" "-c"
                            (format #f "exec -a stumpwm /run/current-system/profile/bin/sbcl --load ~a"
                                    #$stumpwp-load-file))
                     ;; The '--login' option is supported at least by Bash and zsh.
                     (execl shell "i3" "--login" "-c" "exec -a i3 /home/oleg/.guix-profile/bin/i3")))))))
    #~(begin
        (let ((file #$(string-append %home "/.xsession")))
          (copy-file #$xsession-file file)
          (chmod file #o700)))))

(home-environment

 (services
  (append
   (majordomo-mbsync-services "pyhalov")
   (majordomo-mbsync-services "sidorov")
   (majordomo-mbsync-services "fired")
   (majordomo-mbsync-services "pr")
   (majordomo-mbsync-services "noc")

   (majordomo-mbsync-services "alertmanager")
   (majordomo-mbsync-services "git-commits")
   (majordomo-mbsync-services "grafana")
   (majordomo-mbsync-services "healthchecks")
   (majordomo-mbsync-services "issues")
   (majordomo-mbsync-services "jenkins")
   (majordomo-mbsync-services "opensearch")
   (majordomo-mbsync-services "prometheus")
   (majordomo-mbsync-services "security")
   (majordomo-mbsync-services "smartmontools")
   (majordomo-mbsync-services "tracker")
   (majordomo-mbsync-services "ihc-zabbix")
   (majordomo-mbsync-services "web-script")

   (list
    (simple-service 'home-mbsync-wugi-oleg
                    home-mbsync-service-type
                    (mbsync-configuration
                     (imap-accounts
                      (list
                       (mbsync-imap-account-configuration
                        (imap-account "wugi-oleg")
                        (host "imap.wugi.info")
                        (user "oleg@imap.wugi.info")
                        (pass-cmd "gpg -q --for-your-eyes-only --no-tty -d ~/.password-store/vm1.wugi.info/oleg.gpg")
                        (auth-mechs '("LOGIN"))
                        (ssl-type "IMAPS")
                        (certificate-file "/etc/ssl/certs/ca-certificates.crt")
                        (pipeline-depth 50))))
                     (imap-stores
                      (list
                       (mbsync-imap-store-configuration
                        (imap-store "wugi-oleg-remote")
                        (account "wugi-oleg"))))
                     (maildir-stores
                      (list
                       (mbsync-maildir-store-configuration
                        (maildir-store "wugi-oleg-local")
                        (path "~/Maildir/")
                        (inbox "~/Maildir/wugi.info")
                        (sub-folders "Verbatim"))))
                     (channels
                      (list
                       (mbsync-channel-configuration
                        (channel "wugi-oleg")
                        (far ":wugi-oleg-remote:")
                        (near ":wugi-oleg-local:")
                        (patterns '("INBOX"))
                        (sync '("Pull")))))))

    (simple-service 'home-mbsync-wugi-oleg-windows
                    home-mbsync-service-type
                    (mbsync-configuration
                     (imap-accounts
                      (list
                       (mbsync-imap-account-configuration
                        (imap-account "wugi-oleg-windows")
                        (host "imap.wugi.info")
                        (user "oleg-windows@imap.wugi.info")
                        (pass-cmd "gpg -q --for-your-eyes-only --no-tty -d ~/.password-store/vm1.wugi.info/oleg-windows.gpg")
                        (auth-mechs '("LOGIN"))
                        (ssl-type "IMAPS")
                        (certificate-file "/etc/ssl/certs/ca-certificates.crt")
                        (pipeline-depth 50))))
                     (imap-stores
                      (list
                       (mbsync-imap-store-configuration
                        (imap-store "wugi-oleg-windows-remote")
                        (account "wugi-oleg-windows"))))
                     (maildir-stores
                      (list
                       (mbsync-maildir-store-configuration
                        (maildir-store "wugi-oleg-windows-local")
                        (path "~/Maildir/")
                        (inbox "~/Maildir/wugi-oleg-windows")
                        (sub-folders "Verbatim"))))
                     (channels
                      (list
                       (mbsync-channel-configuration
                        (channel "wugi-oleg-windows")
                        (far ":wugi-oleg-windows-remote:")
                        (near ":wugi-oleg-windows-local:")
                        (patterns '("INBOX"))
                        (sync '("Pull")))))))

    (simple-service 'home-mbsync-gmail-wigust
                    home-mbsync-service-type
                    (mbsync-configuration
                     (imap-accounts
                      (list
                       (mbsync-imap-account-configuration
                        (imap-account "gmail")
                        (host "imap.gmail.com")
                        (user "go.wigust@gmail.com")
                        (pass-cmd "gpg -q --for-your-eyes-only --no-tty -d ~/.password-store/myaccount.google.com/apppasswords/go.wigust.gpg")
                        (auth-mechs '("LOGIN"))
                        (ssl-type "IMAPS")
                        (certificate-file "/etc/ssl/certs/ca-certificates.crt")
                        (pipeline-depth 1))))
                     (imap-stores
                      (list
                       (mbsync-imap-store-configuration
                        (imap-store "gmail-remote")
                        (account "gmail"))))
                     (maildir-stores
                      (list
                       (mbsync-maildir-store-configuration
                        (maildir-store "gmail-local")
                        (path "~/Maildir/")
                        (inbox "~/Maildir/INBOX")
                        (sub-folders "Verbatim"))))
                     (channels
                      (list
                       (mbsync-channel-configuration
                        (channel "gmail-inbox")
                        (far ":gmail-remote:")
                        (near ":gmail-local:")
                        (patterns '("INBOX"))
                        (sync '("Pull"))
                        (expunge "None"))
                       (mbsync-channel-configuration
                        (channel "gmail-spam")
                        (far ":gmail-remote:\"[Gmail]/Spam\"")
                        (near ":gmail-local:gmail-spam")
                        (sync '("Pull"))
                        (expunge "None")
                        (create "Near"))))
                     (groups
                      (list
                       (mbsync-group-configuration
                        (group "gmail")
                        (channels '("gmail-inbox"
                                    "gmail-spam")))))))

    (service home-x11-service-type)

    (simple-service 'amtool-config
                    home-files-service-type
                    (list `(".config/amtool/config.yml"
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

    home-chromium-service

    home-bin-service
    home-networking-service

    (simple-service 'looking-glass-wrapper
                    home-files-service-type
                    (list `(".local/bin/looking-glass-client-wrapper"
                            ,(local-file (string-append %project-directory "/dot_local/bin/executable_looking-glass-client-wrapper")
                                         #:recursive? #t))))

    (simple-service 'home-firefox-wrapper-twitch
                    home-files-service-type
                    (list `(".local/bin/firefox-twitch"
                            ,(local-file (string-append %project-directory "/dot_local/bin/executable_firefox-twitch")
                                         #:recursive? #t))))

    (simple-service 'home-firefox-wrapper-react
                    home-files-service-type
                    (list `(".local/bin/firefox-react"
                            ,(local-file (string-append %project-directory "/dot_local/bin/executable_firefox-react")
                                         #:recursive? #t))))

    (simple-service 'home-firefox-wrapper-vnc
                    home-files-service-type
                    (list `(".local/bin/firefox-vnc"
                            ,(local-file (string-append %project-directory "/dot_local/bin/executable_firefox-vnc")
                                         #:recursive? #t))))

    (simple-service 'home-firefox-wrapper-deprecated-default
                    home-files-service-type
                    (list `(".local/bin/firefox-deprecated-default"
                            ,(local-file (string-append %project-directory "/dot_local/bin/executable_firefox-deprecated-default")
                                         #:recursive? #t))))

    (simple-service 'idea-ultimate-wrapper
                    home-files-service-type
                    (list `(".local/bin/idea-ultimate"
                            ,(computed-file
                              "idea-ultimate-wrapper"
                              #~(begin
                                  (with-output-to-file #$output
                                    (lambda ()
                                      (format #t "\
#!/bin/sh
_JAVA_AWT_WM_NONREPARENTING=1 PYTHONPATH='' exec -a \"$0\" ~a/bin/idea-ultimate \"$@\"\n"
                                              #$(string-append %home "/.nix-profile"))))
                                  (chmod #$output #o555))))))

    ;; home-shellcheck-service

    (service stumpwm-service-type
             (let ((config-files
                    '("utils.lisp"
                      "keys.lisp"
                      "nav.lisp"
                      "theme.lisp"
                      "xorg.lisp"
                      "term.lisp"
                      "text-editors.lisp"
                      "repl.lisp"
                      "notify.lisp"
                      "hardware.lisp"
                      "adb.lisp"
                      "admin.lisp"
                      "clipboard.lisp"
                      "screenshoot.lisp"
                      "password.lisp"
                      "trans.lisp"
                      "backup.lisp"
                      "documentation.lisp"
                      "emacs.lisp"
                      "chat.lisp"
                      "mail.lisp"
                      "docker.lisp"
                      "vnc.lisp"
                      "rofi.lisp"
                      "audio.lisp"
                      "mpv.lisp"
                      "streamlink.lisp"
                      "youtube-dl.lisp"
                      "android.lisp"
                      "kodi.lisp"
                      "web.lisp"
                      "time.lisp"
                      "mjru.lisp"
                      "virtualization.lisp"
                      "bittorrent.lisp"
                      "kubernetes.lisp"
                      "disk.lisp"
                      "rest.lisp"
                      "cpu.lisp"
                      "mem.lisp"
                      "imap.lisp"
                      "covid19.lisp"
                      "gpg.lisp"
                      "vpn.lisp"
                      ;; "mode-line.lisp"
                      "display-0.lisp"
                      ;; "display.lisp"
                      ;; "autostart.lisp"
                      ;; "swank.lisp"
                      ;; "gaps.lisp"
                      ;; "windows.lisp"
                      )))
               (stumpwm-configuration
                (init-config
                 `((in-package :stumpwm)

                   (require "asdf")

                   ;; https://discourse.nixos.org/t/fonts-in-nix-installed-packages-on-a-non-nixos-system/5871/9
                   (defvar *fontconfig-file*
                     "FONTCONFIG_FILE=/run/current-system/profile/etc/fonts/fonts.conf")

                   (redirect-all-output
                    (concat
                     (getenv "HOME") "/.local/var/log/stumpwm/" (getenv "DISPLAY") ".log"))

                   ;; (defcommand quassel () ()
                   ;;   (run-shell-command (join (list *fontconfig-file* "/home/oleg/.nix-profile/bin/quassel"))))

                   ;; Tuesday January 3 2005 23:05:25
                   (setq *time-format-string-default* "%A %B %e %Y %k:%M:%S")

                   (setf *startup-message* nil)
                   (setf *message-window-gravity* :center)
                   (setf *input-window-gravity* :center)
                   ,@(map (lambda (config-file)
                            `(load ,(string-append "/home/oleg/.stumpwm.d/" config-file)))
                          config-files)
                   ;; (restore-from-file ,(local-file "/home/oleg/.local/share/chezmoi/dot_stumpwm.d/group-1.lisp"))
                   ))
                (config-files config-files))))

    home-bash-service

    home-mime-service

    home-bind-utils-service

    home-direnv-service

    home-gdb-service

    home-ghci-service

    home-git-service
    home-gita-service

    home-emacs-state-service
    home-emacs-service

    home-groovy-service

    home-gnupg-service

    home-inputrc-service

    home-guile-service

    (simple-service 'keynav-config
                    home-files-service-type
                    (list `(".keynavrc" ,(local-file (string-append %project-directory "/dot_keynavrc")))))

    home-kodi-service

    home-mailcap-service

    home-mongo-service
    home-postgresql-service

    home-mycli-service

    home-nano-service

    home-python-service

    home-sbcl-service

    home-screen-service

    home-tmux-service

    tmuxifier-service

    home-top-service

    (simple-service 'xmodmap-config
                    home-files-service-type
                    (list `(".Xmodmap" ,(local-file (string-append %project-directory "/dot_Xmodmap")))))

    (simple-service 'xresources-config
                    home-files-service-type
                    (list `(".Xresources" ,(local-file (string-append %project-directory "/dot_Xresources")))))

    home-qterminal-service

    (simple-service 'zathura-config
                    home-files-service-type
                    (list `(".config/zathura/zathurarc" ,(local-file (string-append %project-directory "/dot_config/zathura/zathurarc")))))

    home-ripgrep-service

    home-gtk-service
    home-gtkrc-service

    home-alacritty-service
    home-kitty-service

    (simple-service 'feh-config
                    home-files-service-type
                    (list `(".config/feh/buttons" ,(local-file (string-append %project-directory "/dot_config/feh/buttons")))))

    (simple-service 'sway-config
                    home-files-service-type
                    (list `(".config/sway/config" ,(local-file (string-append %project-directory "/dot_config/sway/config")))
                          `(".xkb/symbols/custom" ,(local-file (string-append %project-directory "/dot_xkb/symbols/custom")))))

    (simple-service 'polybar-config
                    home-files-service-type
                    (list `(".config/polybar/config" ,(local-file (string-append %project-directory "/dot_config/polybar/config")))))

    home-youtube-dl-service

    (simple-service 'cava-config
                    home-files-service-type
                    (list `(".config/cava/config" ,(local-file (string-append %project-directory "/dot_config/cava/config")))))

    (simple-service 'termonad-config
                    home-files-service-type
                    (list `(".config/termonad/termonad.hs" ,(local-file (string-append %project-directory "/dot_config/termonad/termonad.hs")))))

    home-nix-service

    home-wireplumber-config-service

    home-mpv-service

    (simple-service 'cagebreak-config
                    home-files-service-type
                    (list `(".config/cagebreak/config" ,(local-file (string-append %project-directory "/dot_config/cagebreak/config")))))

    (simple-service 'vis-config
                    home-files-service-type
                    (list `(".config/vis/config" ,(local-file (string-append %project-directory "/dot_config/vis/config")))))

    (simple-service 'dunst-config
                    home-files-service-type
                    (list `(".config/dunst/dunstrc" ,(local-file (string-append %project-directory "/dot_config/dunst/dunstrc")))))


    ;; TODO: Add those
    ;; dot_config/guile/mariadb.scm
    ;; dot_config/mjru/encrypted_config.scm
    ;; dot_config/mjru/encrypted_firefox.scm

    ;; Symlinking to store breaks espanso, so use files instead.
    (simple-service 'espanso-config
                    home-activation-service-type
                    #~(invoke
                       #$(program-file
                          "espanso-config"
                          (with-imported-modules '((ice-9 match))
                            #~(begin
                                (use-modules (ice-9 match))
                                (for-each (match-lambda ((destination source)
                                                         (let ((destination-full-path (string-append #$%home "/." destination)))
                                                           (when (file-exists? destination-full-path)
                                                             (chmod destination-full-path #o644))
                                                           (copy-file source destination-full-path))))
                                          (list `("config/espanso/default.yml" ,#$(local-file (string-append %project-directory "/dot_config/espanso/default.yml")))
                                                ;; TODO: Add `("config/espanso/user/home.yml.tmpl" ,(local-file (string-append %project-directory "/dot_config/espanso/user/home.yml.tmpl")))
                                                `("config/espanso/user/systemd.yml" ,#$(local-file (string-append %project-directory "/dot_config/espanso/user/systemd.yml")))
                                                `("config/espanso/user/juniper.yml" ,#$(local-file (string-append %project-directory "/dot_config/espanso/user/juniper.yml")))
                                                `("config/espanso/user/mysql.yml" ,#$(local-file (string-append %project-directory "/dot_config/espanso/user/mysql.yml")))
                                                `("config/espanso/user/nix.yml" ,#$(local-file (string-append %project-directory "/dot_config/espanso/user/nix.yml")))
                                                `("config/espanso/user/mjru.yml" ,#$(local-file (string-append %project-directory "/dot_config/espanso/user/mjru.yml"))))))))))

    (simple-service 'sshrc-config
                    home-files-service-type
                    (list `(".sshrc" ,(local-file (string-append %project-directory "/dot_sshrc")))
                          `(".sshrc.d/.bashrc" ,(local-file (string-append %project-directory "/dot_sshrc.d/dot_bashrc")))
                          `(".sshrc.d/.tmux.conf" ,(local-file (string-append %project-directory "/dot_sshrc.d/dot_tmux.conf")))))

    (simple-service 'vnc-config
                    home-files-service-type
                    (list `(".vnc/default.tigervnc" ,(local-file (string-append %project-directory "/private_dot_vnc/default.tigervnc")))
                          `(".vnc/xstartup" ,(local-file (string-append %project-directory "/private_dot_vnc/executable_xstartup") #:recursive? #t))
                          `(".vnc/xstartup-firefox" ,(local-file (string-append %project-directory "/private_dot_vnc/executable_xstartup-firefox") #:recursive? #t))
                          `(".vnc/xstartup-quassel" ,(local-file (string-append %project-directory "/private_dot_vnc/executable_xstartup-quassel") #:recursive? #t))
                          `(".vnc/xstartup-ratpoison" ,(local-file (string-append %project-directory "/private_dot_vnc/executable_xstartup-ratpoison") #:recursive? #t))
                          `(".vnc/xstartup-stumpwm" ,(local-file (string-append %project-directory "/private_dot_vnc/executable_xstartup-stumpwm") #:recursive? #t))
                          `(".vnc/xstartup-twm" ,(local-file (string-append %project-directory "/private_dot_vnc/executable_xstartup-twm") #:recursive? #t))))

    (simple-service 'xsession-config
                    home-activation-service-type
                    xsession-config-file)

    home-parallel-service

    (simple-service 'msmtp-config
                    home-activation-service-type
                    #~(begin
                        (define %home
                          (and=> (getenv "HOME")
                                 (lambda (home)
                                   home)))
                        (add-to-load-path (string-append %home "/.local/share/chezmoi/dotfiles"))
                        (use-modules (ice-9 format)
                                     (guile pass))
                        (define msmtp-config
                          (string-append %home "/.msmtprc"))
                        (call-with-output-file msmtp-config
                          (lambda (port)
                            (format port "\
# Set default values for all following accounts.
defaults
auth           on
tls            on
tls_trust_file /etc/ssl/certs/ca-certificates.crt
logfile        ~a/.msmtp.log

# Gmail
account        gmail
host           smtp.gmail.com
port           587
from           go.wigust@gmail.com
user           go.wigust
password       ~a

# Majordomo
account        majordomo-pyhalov
host           smtp.majordomo.ru
port           587
from           pyhalov@majordomo.ru
user           pyhalov@majordomo.ru
password       ~a

# Majordomo NOC
account        majordomo-noc
host           smtp.majordomo.ru
port           587
from           noc@majordomo.ru
user           noc@majordomo.ru
password       ~a

# Set a default account
account default : gmail
"
                                    %home
                                    (pass "myaccount.google.com/apppasswords/go.wigust")
                                    (pass "majordomo/private/newmail.majordomo.ru/pyhalov@majordomo.ru")
                                    (pass "majordomo/private/router.majordomo.ru/noc@majordomo.ru"))))
                        (chmod msmtp-config #o600)))

    (simple-service 'netrc-config
                    home-activation-service-type
                    (let ((file
                           (scheme-file
                            "netrc.scm"
                            #~(begin
                                (let ((%home
                                       (and=> (getenv "HOME")
                                              (lambda (home)
                                                home))))
                                  (add-to-load-path (string-append %home "/.local/share/chezmoi/dotfiles"))
                                  (use-modules (ice-9 format)
                                               (ice-9 match)
                                               (guile pass))
                                  (call-with-output-file (string-append %home "/.netrc")
                                    (lambda (port)
                                      (for-each
                                       (match-lambda
                                         ((net (machine m)
                                               (login l)
                                               (password p))
                                          (format port "~%machine ~a~%" m)
                                          (format port "login ~a~%" l)
                                          (format port "password ~a~%" (pass p))))
                                       '((net
                                          (machine "api.github.com")
                                          (login "wigust")
                                          (password "github/tokens/api/wigust"))
                                         (net
                                          (machine "uploads.github.com")
                                          (login "wigust")
                                          (password "github/tokens/api/wigust"))
                                         (net
                                          (machine "imap.yandex.ru")
                                          (login "houdinihar")
                                          (password "email/yandex.ru/houdinihar"))
                                         (net
                                          (machine "imap.rambler.ru")
                                          (login "houdinihar")
                                          (password "email/rambler/houdinihar"))
                                         (net
                                          (machine "imap.majordomo.ru")
                                          (login "pyhalov")
                                          (password "majordomo/private/newmail.majordomo.ru/pyhalov@majordomo.ru"))
                                         (net
                                          (machine "localhost")
                                          (login "oleg")
                                          (password "localhost/imap/oleg"))
                                         (net
                                          (machine "bareos.intr")
                                          (login "netcfg")
                                          (password "majordomo/public/172.16.103.111/netcfg"))
                                         (net
                                          (machine "pop3.hoster24.ru")
                                          (login "pop3")
                                          (password "majordomo/public/hoster24.ru/pop3"))
                                         (net
                                          (machine "172.16.103.111")
                                          (login "netcfg")
                                          (password "majordomo/public/172.16.103.111/netcfg")))))))))))
                      #~(begin (primitive-load #$file))))

    (service home-mcron-service-type)
    (service nix-delete-generations-service-type
             (nix-delete-generations-configuration
              (schedule '(next-hour '(21)))))
    (service guix-delete-generations-service-type
             (guix-delete-generations-configuration
              (schedule '(next-hour '(21)))
              (period "1m")))

    (service kubernetes-service-type)
    (service billing2-service-type)

    (simple-service 'ansible-config
                    home-files-service-type
                    (append (list `(,".ansible.cfg" ,(local-file (string-append %project-directory "/dot_ansible.cfg"))))
                            (map (lambda (file-name)
                                   `(,(string-append ".ansible/plugins/modules/" file-name)
                                     ,(local-file (string-append %project-directory
                                                                 "/dot_ansible/plugins/modules/"
                                                                 file-name))))
                                 '("guix_package.py"
                                   "guix_pull.py"))))

    (simple-service 'ansible-hosts-config
                    home-activation-service-type
                    (with-extensions (list guile-json-4)
                      (with-imported-modules (source-module-closure '((json builder)))
                        #~(begin
                            (add-to-load-path (string-append #$%home "/.local/share/chezmoi/dotfiles"))
                            (use-modules (ice-9 rdelim)
                                         (ice-9 popen)
                                         (guile pass)
                                         (json builder)
                                         (ice-9 match)
                                         (srfi srfi-41))

                            (define majordomo-office
                              '(;; ("Admin-1" "172.16.107.21" "18:c0:4d:f9:c0:b6" "Home segment" "Wired" "100 Mbit/s" "Port 1")
                                ;; ("Admin-2" "172.16.107.22" "18:c0:4d:f9:c1:9a" "Offline")
                                ;; ("Admin-3" "172.16.107.23" "18:c0:4d:f9:c0:bb" "Home segment" "Wired" "100 Mbit/s" "Port 1")
                                ;; ("dev-1" "172.16.107.11" "18:c0:4d:f9:c1:99" "Home segment" "Wired" "100 Mbit/s" "Port 1")
                                ;; ("dev-2" "172.16.107.12" "18:c0:4d:f9:c1:9b" "Home segment" "Wired" "100 Mbit/s" "Port 1")
                                ;; ("Info-1" "172.16.107.31" "18:c0:4d:f9:b3:63" "Home segment" "Wired" "100 Mbit/s" "Port 1")
                                ;; ("Info-2" "172.16.107.32" "18:c0:4d:f9:c0:fe" "Home segment" "Wired" "100 Mbit/s" "Port 1")
                                ;; ("iPhone-Gennadiy via Keenetic Air (KN-1611)" "172.16.107.143" "ce:97:72:a9:1f:d6" "Home segment" "5 GHz Wi-Fi" " 130 Mbit/s WPA2" "ac/k/v 2x2 20 MHz")
                                ;; ("SPA122" "172.16.107.8" "88:75:56:07:73:92" "Home segment" "Wired" "0 Mbit/s" "Port 1")
                                ("sup1" "172.16.107.41" "18:c0:4d:f9:c0:b8" "Home segment" "Wired" "100 Mbit/s" "Port 1")
                                ("sup2" "172.16.107.42" "18:c0:4d:f9:c0:bc" "Home segment" "Wired" "100 Mbit/s" "Port 1")
                                ("sup3" "172.16.107.43" "18:c0:4d:f9:c0:f9" "Home segment" "Wired" "100 Mbit/s" "Port 1")
                                ("sup4" "172.16.107.44" "18:c0:4d:f9:c0:83" "Home segment" "Wired" "100 Mbit/s" "Port 1")))

                            (call-with-output-file #$(string-append %home "/.ansible-hosts")
                                                   (lambda (port)
                                                     (scm->json
                                                      `(("vps"
                                                         ("vars"
                                                          ("ansible_ssh_user" . "opc")
                                                          ("ansible_ssh_private_key_file"
                                                           .
                                                           "~/.ssh/id_rsa_oracle"))
                                                         ("hosts" ("oracle" . null)))
                                                        ("vm"
                                                         ("vars"
                                                          ("ansible_ssh_user" . "root")
                                                          ("ansible_python_interpreter"
                                                           .
                                                           "/usr/bin/python3"))
                                                         ("children"
                                                          ("majordomo_office"
                                                           ("hosts"
                                                            ,@(map ((@@ (ice-9 match) match-lambda)
                                                                    ((name ip mac etc ...)
                                                                     (cons ip 'null)))
                                                                   majordomo-office))
                                                           ("vars"
                                                            ("ansible_ssh_user" . "root")
                                                            ("ansible_python_interpreter" . "/run/current-system/sw/bin/python3")))
                                                          ("mjru"
                                                           ("hosts"
                                                            ("78.108.87.99" . null)
                                                            ("78.108.87.50" . null)
                                                            ("78.108.86.20" . null)
                                                            ("78.108.86.111" . null)
                                                            ("78.108.82.130" . null)
                                                            ("178.250.247.88" . null)
                                                            ("178.250.247.60" . null)
                                                            ("178.250.246.69" . null)
                                                            ("178.250.246.123" . null)
                                                            ("178.250.245.80" . null)
                                                            ("178.250.244.239" . null)))
                                                          ("ihc"
                                                           ("vars"
                                                            ("ansible_ssh_user" . "pyhalov")
                                                            ("ansible_ssh_private_key_file" . "~/.ssh/id_rsa_ihc_pyhalov"))
                                                           ("hosts"
                                                            ("kvm-nvme1.majordomo.ru" . null)
                                                            ("kvm-nvme101.majordomo.ru" . null)
                                                            ("kvm-nvme102.majordomo.ru" . null)
                                                            ("kvm-nvme201.majordomo.ru" . null)
                                                            ("kvm-nvme202.majordomo.ru" . null)
                                                            ("kvm-nvme203.majordomo.ru" . null)))
                                                          ("kubernetes"
                                                           ("children"
                                                            ("kubernetes_1"
                                                             ("hosts"
                                                              ,@(map (lambda (number)
                                                                       `(,(string-append "kube" (number->string number) ".intr") . null))
                                                                     (stream->list (stream-range 1 44)))))
                                                            ("kubernetes_2"
                                                             ("hosts"
                                                              ,@(map (lambda (number)
                                                                       `(,(string-append "kube" (number->string number) ".intr") . null))
                                                                     (stream->list (stream-range 5000 5025))))))
                                                           ("vars"
                                                            ("ansible_ssh_user" . "root")
                                                            ("ansible_python_interpreter" . "/run/current-system/sw/bin/python3")))
                                                          ("glpi" ("hosts" ("178.250.244.239" . null)))
                                                          ("docker"
                                                           ("hosts"
                                                            ("78.108.87.99" . null)
                                                            ("78.108.86.20" . null)
                                                            ("178.250.246.123" . null))))
                                                         )
                                                        ("majordomo"
                                                         ("hosts"
                                                          ("zabbix.intr" . null)
                                                          ("ci.intr" . null)
                                                          ("bareos.intr" . null)
                                                          ("archive.intr" . null)
                                                          ("backup.intr" . null))
                                                         ("children"
                                                          ("web"
                                                           ("hosts"
                                                            ("web37.intr" . null)
                                                            ("web36.intr" . null)
                                                            ("web35.intr" . null)
                                                            ("web34.intr" . null)
                                                            ("web33.intr" . null)
                                                            ("web32.intr" . null)
                                                            ("web31.intr" . null)
                                                            ("web30.intr" . null)
                                                            ("web29.intr" . null)
                                                            ("web28.intr" . null)
                                                            ("web27.intr" . null)
                                                            ("web26.intr" . null)
                                                            ("web25.intr" . null)
                                                            ;; ("web24.intr" . null)
                                                            ("web23.intr" . null)
                                                            ("web22.intr" . null)
                                                            ("web21.intr" . null)
                                                            ("web20.intr" . null)
                                                            ("web19.intr" . null)
                                                            ("web18.intr" . null)
                                                            ("web17.intr" . null)
                                                            ("web16.intr" . null)
                                                            ("web15.intr" . null)))
                                                          ("deprecated_web"
                                                           ("hosts"
                                                            #$@%ansible-majordomo-deprecated-webs))
                                                          ("vpn"
                                                           ("hosts"
                                                            ("ns1-dh.intr" . null)
                                                            ("galera-backup.intr" . null)))
                                                          ("swarm"
                                                           ("hosts"
                                                            ("dh5-mr.intr" . null)
                                                            ("dh4-mr.intr" . null)
                                                            ("dh3-mr.intr" . null)
                                                            ("dh2-mr.intr" . null)
                                                            ("dh1-mr.intr" . null)))
                                                          ("router"
                                                           ("children"
                                                            ("office"
                                                             ("hosts"
                                                              ("router2.intr" . null)
                                                              ("router1.intr" . null)))
                                                            ("freebsd"
                                                             ("vars"
                                                              ("ansible_python_interpreter"
                                                               .
                                                               "/usr/local/bin/python2"))
                                                             ("hosts" ("router-miran1.intr" . null)))))
                                                          ("redis"
                                                           ("hosts"
                                                            ("hms02-mr.intr" . null)
                                                            ("hms01-mr.intr" . null)))
                                                          ("ns"
                                                           ("hosts"
                                                            ("ns2-mr.intr" . null)
                                                            ("ns2-dh.intr" . null)
                                                            ("ns1-mr.intr" . null)
                                                            ("ns1-dh.intr" . null)))
                                                          ("nginx"
                                                           ("hosts"
                                                            ("nginx3.intr" . null)
                                                            ("nginx2.intr" . null)
                                                            ("nginx1.intr" . null)))
                                                          ("mongo"
                                                           ("hosts"
                                                            ("hms03-mr.intr" . null)
                                                            ("hms02-mr.intr" . null)
                                                            ("hms01-mr.intr" . null)))
                                                          ("miran"
                                                           ("children"
                                                            ("rack4"
                                                             ("hosts"
                                                              ("zabbix.intr" . null)
                                                              ("web18.intr" . null)
                                                              ("web15.intr" . null)
                                                              ("staff.intr" . null)
                                                              ("router-miran1.intr" . null)
                                                              ("mail-checker2.intr" . null)
                                                              ("kvm14.intr" . null)
                                                              ("galera3.intr" . null)
                                                              ("galera2.intr" . null)
                                                              ("galera1.intr" . null)
                                                              ("dh2.intr" . null)
                                                              ("bareos.intr" . null)))
                                                            ("rack3"
                                                             ("hosts"
                                                              ("web37.intr" . null)
                                                              ("web34.intr" . null)
                                                              ("web33.intr" . null)
                                                              ("web32.intr" . null)
                                                              ("web31.intr" . null)
                                                              ("web30.intr" . null)
                                                              ("web29.intr" . null)
                                                              ("web28.intr" . null)
                                                              ("web27.intr" . null)
                                                              ("web26.intr" . null)
                                                              ("web25.intr" . null)
                                                              ;; ("web24.intr" . null)
                                                              ("web23.intr" . null)
                                                              ("web22.intr" . null)
                                                              ("web20.intr" . null)
                                                              ("web19.intr" . null)
                                                              ("web17.intr" . null)
                                                              ("web16.intr" . null)
                                                              ("ns2-mr.intr" . null)
                                                              ("kvm2.intr" . null)
                                                              ("kvm15.intr" . null)
                                                              ("jenkins.intr" . null)))
                                                            ("rack2"
                                                             ("hosts"
                                                              ("webmail2.intr" . null)
                                                              ("webmail1.intr" . null)
                                                              ("web36.intr" . null)
                                                              ("web35.intr" . null)
                                                              ("web21.intr" . null)
                                                              ("smtp-staff.intr" . null)
                                                              ("ns1-mr.intr" . null)
                                                              ("nginx2.intr" . null)
                                                              ("nginx1.intr" . null)
                                                              ("kvm37.intr" . null)
                                                              ("hms02-mr.intr" . null)
                                                              ("galera-backup.intr" . null)
                                                              ("dh4.intr" . null)
                                                              ("dh1.intr" . null)
                                                              ("chef-server.intr" . null)
                                                              ("buka2-new.intr" . null)
                                                              ("archive.intr" . null)))
                                                            ("rack1"
                                                             ("hosts"
                                                              ("pop5.intr" . null)
                                                              ("mx1.intr" . null)
                                                              ("hms01-mr.intr" . null)
                                                              ("fluentd.intr" . null)
                                                              ("dh3.intr" . null)))))
                                                          ("mail"
                                                           ("children"
                                                            ("spam"
                                                             ("hosts"
                                                              ("mail-checker2.intr" . null)
                                                              ("mail-checker1.intr" . null)))
                                                            ("smtp"
                                                             ("hosts"
                                                              ("webmail2.intr" . null)
                                                              ("webmail1.intr" . null)))
                                                            ("pop"
                                                             ("hosts"
                                                              ("pop5.intr" . null)
                                                              ("pop1.intr" . null)))
                                                            ("mx"
                                                             ("hosts"
                                                              ("nginx2.intr" . null)
                                                              ("nginx1.intr" . null)))))
                                                          ("kvm"
                                                           ("hosts"
                                                            ("kvm9.intr" . null)
                                                            ("kvm6.intr" . null)
                                                            ("kvm5.intr" . null)
                                                            ("kvm37.intr" . null)
                                                            ("kvm35.intr" . null)
                                                            ("kvm34.intr" . null)
                                                            ("kvm33.intr" . null)
                                                            ("kvm32.intr" . null)
                                                            ("kvm31.intr" . null)
                                                            ("kvm30.intr" . null)
                                                            ("kvm29.intr" . null)
                                                            ("kvm28.intr" . null)
                                                            ("kvm27.intr" . null)
                                                            ("kvm26.intr" . null)
                                                            ("kvm25.intr" . null)
                                                            ("kvm24.intr" . null)
                                                            ("kvm23.intr" . null)
                                                            ("kvm22.intr" . null)
                                                            ("kvm21.intr" . null)
                                                            ("kvm20.intr" . null)
                                                            ("kvm2.intr" . null)
                                                            ("kvm19.intr" . null)
                                                            ("kvm17.intr" . null)
                                                            ("kvm16.intr" . null)
                                                            ("kvm15.intr" . null)
                                                            ("kvm14.intr" . null)
                                                            ("kvm13.intr" . null)
                                                            ("kvm12.intr" . null)
                                                            ("kvm10.intr" . null)
                                                            ("kvm1.intr" . null)
                                                            ("c-11122.intr" . null)))
                                                          ("jenkins" ("hosts" ("jenkins.intr" . null)))
                                                          ("galera"
                                                           ("hosts"
                                                            ("galera1.intr" . null)
                                                            ("galera2.intr" . null)
                                                            ("galera3.intr" . null)))
                                                          ("elk"
                                                           ("hosts"
                                                            ("pop5.intr" . null)
                                                            ("fluentd.intr" . null)
                                                            ("chef-server.intr" . null)))
                                                          ("datahouse"
                                                           ("hosts"
                                                            ("kvm1.intr" . null)
                                                            ("kvm2.intr" . null)
                                                            ("kvm5.intr" . null)
                                                            ("kvm6.intr" . null)
                                                            ("kvm7.intr" . null)
                                                            ("kvm9.intr" . null)
                                                            ("kvm10.intr" . null)
                                                            ("kvm11.intr" . null)
                                                            ("kvm12.intr" . null)
                                                            ("kvm13.intr" . null)
                                                            ("kvm14.intr" . null)
                                                            ("kvm15.intr" . null)
                                                            ("kvm16.intr" . null)
                                                            ("kvm17.intr" . null)
                                                            ;; ("kvm18.intr" . null)
                                                            ("kvm19.intr" . null)
                                                            ("kvm20.intr" . null)
                                                            ("kvm21.intr" . null)
                                                            ("kvm22.intr" . null)
                                                            ("kvm23.intr" . null)
                                                            ("kvm24.intr" . null)
                                                            ("kvm25.intr" . null)
                                                            ("kvm26.intr" . null)
                                                            ("kvm27.intr" . null)
                                                            ("kvm28.intr" . null)
                                                            ("kvm29.intr" . null)
                                                            ("kvm34.intr" . null)
                                                            ("kvm33.intr" . null)
                                                            ("kvm32.intr" . null)
                                                            ("kvm31.intr" . null)
                                                            ("kvm30.intr" . null)
                                                            ("kvm35.intr" . null)
                                                            ("kvm37.intr" . null)))))
                                                        ("guix"
                                                         ("vars"
                                                          ("ansible_python_interpreter"
                                                           .
                                                           "/home/oleg/.guix-profile/bin/python3"))
                                                         ("children"
                                                          ("local"
                                                           ("vars" ("ansible_connection" . "local"))
                                                           ("hosts" ("localhost" . null)))
                                                          ;; ("guix_work" ("hosts" ("ws1.wugi.info" . null)))
                                                          ("guix_vm"
                                                           ("hosts"
                                                            ("vm1.wugi.info" . null)
                                                            ("vm2.wugi.info" . null))))))
                                                      port
                                                      #:pretty #t)))))))

    (service home-openssh-service-type %home-openssh-configuration)

    ;; XXX: missing home-ssh-configuration
    ;; (service home-ssh-service-type
    ;;          (home-ssh-configuration
    ;;           (extra-config
    ;;            (list
    ;;             (ssh-host "savannah"
    ;;                   '((compression . #f)))))))

    (service nix-build-service-type
             (nix-build-configurations
              (configurations
               (append
                (map
                 (lambda (hostname)
                   (nix-build-configuration
                    (name hostname)
                    (git-project git-project-nixos-pop)))
                 '("pop1" "pop5"))
                (map
                 (lambda (hostname)
                   (nix-build-configuration
                    (name hostname)
                    (git-project git-project-nixos-monitoring)))
                 '("staff"))
                (map
                 (lambda (hostname)
                   (nix-build-configuration
                    (name hostname)
                    (git-project git-project-nixos-ns)))
                 '("ns1-mr" "ns2-mr" "ns1-dh" "ns2-dh"))
                (map
                 (lambda (hostname)
                   (nix-build-configuration
                    (name hostname)
                    (git-project git-project-nixos-web)))
                 '("web30"))
                (map
                 (lambda (hostname)
                   (nix-build-configuration
                    (name hostname)
                    (git-project git-project-nixos-jenkins)))
                 '("jenkins"))))))

    ;; XXX: Make sure ~/.ssh/known_hosts provides ssh-rsa host key algorithm,
    ;; so ssh-exporter works properly.
    (service home-prometheus-ssh-exporter-service-type
             (prometheus-ssh-exporter-configuration
              (config-file
               (computed-file
                "ssh-exporter.json"
                (with-extensions (list guile-json-4)
                  (with-imported-modules (source-module-closure '((json builder)))
                    #~(begin
                        (use-modules (json builder)
                                     (ice-9 rdelim))
                        (define %home #$%home)
                        (with-output-to-file #$output
                          (lambda ()
                            (scm->json
                             `(("modules"
                                ("default"
                                 ("user" . "oleg")
                                 ("timeout" . 5)
                                 ("private_key" . ,(string-append %home "/.ssh/id"))
                                 ("known_hosts" . ,(string-append %home "/.ssh/known_hosts"))
                                 ("host_key_algorithms" . #("ssh-ed25519"))
                                 ("command" . "uptime"))
                                ("id_rsa"
                                 ("user" . "oleg")
                                 ("timeout" . 5)
                                 ("private_key" . ,(string-append %home "/.ssh/id_rsa"))
                                 ("known_hosts" . ,(string-append %home "/.ssh/known_hosts"))
                                 ("command" . "uptime"))
                                ("majordomo-eng"
                                 ("user" . "eng")
                                 ("timeout" . 5)
                                 ("private_key" . ,(string-append %home "/.ssh/id_rsa_majordomo_eng"))
                                 ("known_hosts" . ,(string-append %home "/.ssh/known_hosts"))
                                 ("command" . "uptime"))
                                ("majordomo-net"
                                 ("user" . "root")
                                 ("timeout" . 5)
                                 ("private_key" . ,(string-append %home "/.ssh/id_rsa_majordomo_eng"))
                                 ("known_hosts" . ,(string-append %home "/.ssh/known_hosts"))
                                 ("command" . "uptime"))))))))))))))

    ;; oleg@guixsd ~/.local/share/chezmoi$ command guix home -L dotfiles/guixsd/modules -L ~/src/engstrand-config-home-service-dwl-guile reconfigure dotfiles/guixsd/home.scm
    ;; (service home-dwl-guile-service-type
    ;;          (home-dwl-guile-configuration
    ;;           (package-transform? #f)
    ;;           (auto-start? #f)))

    ;; (simple-service 'add-dwl-guile-keybinding home-dwl-guile-service-type
    ;;  (modify-dwl-guile-config
    ;;   (config =>
    ;;           (dwl-config
    ;;            (inherit config)
    ;;            (keys
    ;;             (append
    ;;              (let ((ponymix (file-append ponymix "/bin/ponymix")))
    ;;                (list
    ;;                 (dwl-key
    ;;                  (key "s-w")
    ;;                  (action `(dwl:spawn "firefox")))
    ;;                 (dwl-key
    ;;                  (key "<XF86AudioMute>")
    ;;                  (action `(dwl:spawn ,ponymix "toggle")))
    ;;                 (dwl-key
    ;;                  (key "<XF86AudioLowerVolume>")
    ;;                  (action `(dwl:spawn ,ponymix "decrease" "5")))
    ;;                 (dwl-key
    ;;                  (key "<XF86AudioRaiseVolume>")
    ;;                  (action `(dwl:spawn ,ponymix "increase" "5")))
    ;;                 (dwl-key
    ;;                  (key "S-s-=")
    ;;                  (action `(dwl:spawn ,(file-append pavucontrol "/bin/pavucontrol"))))
    ;;                 (dwl-key
    ;;                  (key "s-e")
    ;;                  (action `(dwl:spawn "emacs")))))
    ;;              (dwl-config-keys config)))
    ;;            (terminal `(,(file-append alacritty "/bin/alacritty")))
    ;;            (menu `(,(file-append bemenu "/bin/bemenu-run")))))))

    ;; TODO: Requires for a reimplementation after switching to Wayland.
    ;;
    ;; (simple-service
    ;;  'auto-shutdown-cron-jobs
    ;;  home-mcron-service-type
    ;;  (list
    ;;   #~(job
    ;;      '(next-hour)
    ;;      #$(program-file
    ;;         "schedule-power"
    ;;         #~(begin
    ;;             (system*
    ;;              #$(local-file (string-append %project-directory "/dot_local/bin/executable_schedule-power")
    ;;                            #:recursive? #t)))))))

    (service home-dbus-service-type)

    (service home-pipewire-service-type)

    (service home-scream-service-type
             (scream-configuration
              (interface "br154.154")))

    (service home-sway-service-type
             (sway-configuration
              (environment-variables
               '("WLR_BACKENDS=headless,libinput"))))

    (service home-wayvnc-service-type
             (wayvnc-configuration
              (wayvnc "/home/oleg/bin/wayvnc")
              (environment-variables
               '("WAYLAND_DISPLAY=wayland-1"))))))))
