(define-module (wugi home config guixsd)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages tmux)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix profiles)
  #:use-module (ice-9 rdelim)
  #:use-module (json)

  #:use-module (gnu packages base)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages wm)

  #:use-module (wugi home config)
  #:use-module (wugi home services ansible)
  #:use-module (wugi home services cisco)
  #:use-module (wugi home services desktop)
  #:use-module (wugi home services gdb)
  #:use-module (wugi home services emacs)
  #:use-module (wugi home services mail)
  #:use-module (wugi home services monitoring)
  #:use-module (wugi home services nix)
  #:use-module (wugi home services package-management)
  #:use-module (wugi home services shell)
  #:use-module (wugi home services version-control)
  #:use-module (wugi home services terminals)
  #:use-module (wugi home services tmux)
  #:use-module (wugi home services linux)
  #:use-module (wugi home services haskell-apps)
  #:use-module (wugi home services gtk)
  #:use-module (wugi home services rust-apps)
  #:use-module (wugi home services lisp)
  #:use-module (wugi home services python)
  #:use-module (wugi home services nano)
  #:use-module (wugi home services dns)
  #:use-module (wugi home services web)
  #:use-module (wugi home services gnupg)
  #:use-module (wugi home services groovy)
  #:use-module (wugi home services guile)
  #:use-module (wugi home services kodi)
  #:use-module (wugi home services databases)
  #:use-module (wugi home services mime)
  #:use-module (wugi home services video)
  #:use-module (wugi home services kubernetes)
  #:use-module (wugi home services majordomo billing2)
  #:use-module (wugi home services audio)

  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages mail)
  #:use-module (wugi guile pass)
  #:use-module (wugi utils)

  #:export (%guixsd-home-environment))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define .bashrc
  (string-append %home "/src/cgit.wugi.info/wigust/dotfiles/dot_bashrc"))

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
                         (list "gpg"
                               "--pinentry-mode" "loopback"
                               "--quiet"
                               "--for-your-eyes-only"
                               "--no-tty"
                               "--decrypt"
                               (string-append "~/.password-store/" (pass-private-or-public name) ".gpg")))))))
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

(define (%guixsd-home-environment)
  (home-environment
    (packages
     (list git
           `(,git "send-email")
           tmux))
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
                           (pass-cmd "gpg --pinentry-mode loopback --quiet --for-your-eyes-only --no-tty --decrypt ~/.password-store/vm1.wugi.info/oleg.gpg")
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
                           (pass-cmd "gpg --pinentry-mode loopback --quiet --for-your-eyes-only --no-tty --decrypt ~/.password-store/vm1.wugi.info/oleg-windows.gpg")
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
                           (pass-cmd "gpg --pinentry-mode loopback --quiet --for-your-eyes-only --no-tty --decrypt ~/.password-store/myaccount.google.com/apppasswords/go.wigust.gpg")
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

       (simple-service 'looking-glass-wrapper
                       home-files-service-type
                       (list `(".local/bin/looking-glass-client-wrapper"
                               ,(local-file (string-append %distro-directory "/dot_local/bin/looking-glass-client-wrapper")
                                            #:recursive? #t))))

       (simple-service 'home-firefox-wrapper-twitch
                       home-files-service-type
                       (list `(".local/bin/firefox-twitch"
                               ,(local-file (string-append %distro-directory "/dot_local/bin/firefox-twitch")
                                            #:recursive? #t))))

       (simple-service 'home-firefox-wrapper-react
                       home-files-service-type
                       (list `(".local/bin/firefox-react"
                               ,(local-file (string-append %distro-directory "/dot_local/bin/firefox-react")
                                            #:recursive? #t))))

       (simple-service 'home-firefox-wrapper-vnc
                       home-files-service-type
                       (list `(".local/bin/firefox-vnc"
                               ,(local-file (string-append %distro-directory "/dot_local/bin/firefox-vnc")
                                            #:recursive? #t))))

       (simple-service 'home-firefox-wrapper-deprecated-default
                       home-files-service-type
                       (list `(".local/bin/firefox-deprecated-default"
                               ,(local-file (string-append %distro-directory "/dot_local/bin/firefox-deprecated-default")
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

       home-bash-service

       (service home-bash-service-type
                (home-bash-configuration
                  (bashrc
                   (list
                    (local-file
                     (string-append %distro-directory "/dot_bashrc"))))
                  (environment-variables
                   `(("PATH" .
                      ,(string-append "${HOME}/bin"
                                      ":" "${HOME}/.local/bin"
                                      ;; ":" "$(/usr/bin/env --ignore-environment sh --norc --noprofile -c 'unset PATH; export HOME=/home/oleg; export USER=oleg; source /etc/profile; printf $PATH')"
                                      ":" "${HOME}/go/bin"
                                      ":" "${HOME}/.npm-global/bin"
                                      ":" "/opt/gradle/bin"
                                      ":" "${HOME}/perl5/bin"
                                      ;; ":" "${HOME}/.nix-profile/lib/openjdk/bin"
                                      ;; ":" "${HOME}/.nix-profile/bin"
                                      ":" "$PATH"))
                     ("LC_TIME" . "en_GB.UTF-8")
                     ("LANG" . "en_US.UTF-8")

                     ("CHICKEN_REPOSITORY" . "${HOME}/.eggs/lib/chicken/8")
                     ("CHICKEN_DOC_REPOSITORY" . "${HOME}/.eggs/share/chicken-doc")

                     ("BROWSER" . "icecat")
                     ("INFOPATH" . "${HOME}/src/codeberg.org/guix/guix/doc${INFOPATH:+:}$INFOPATH")
                     ("GUILE_WARN_DEPRECATED" . "no")

                     ;; Fix mouse wheel in gtk3
                     ;; https://github.com/stumpwm/stumpwm/wiki/FAQ
                     ("GDK_CORE_DEVICE_EVENTS" . "1")

                     ("QT_QPA_PLATFORMTHEME" . "gtk2")
                     ("GUILE_LOAD_PATH" . "${HOME}/.config:${GUILE_LOAD_PATH}")
                     ("RIPGREP_CONFIG_PATH" . "${HOME}/.config/ripgrep/ripgreprc")
                     ("SSHRC_BECOME" . "yes")
                     ("GRADLE_HOME" . "/opt/gradle")
                     ("PYTHONSTARTUP" . "${HOME}/.pythonrc")
                     ("TMUXIFIER_LAYOUT_PATH" . "${HOME}/.tmuxifier-layouts")
                     ("EDITOR" . "emacsclient -nw -c")
                     ("MANWIDTH" . "80")
                     ("PERL5LIB" . "${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}")
                     ("PERL_LOCAL_LIB_ROOT" . "${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}")
                     ("PERL_MB_OPT" . "--install_base ${HOME}/perl5")
                     ("PERL_MM_OPT" . "INSTALL_BASE=${HOME}/perl5")))

                  ;; ;; GUIX_PROFILE variable used in my custom
                  ;; ;; .bashrc file.
                  ;; (variables '(("GUIX_PROFILE" . "${HOME}/.guix-home/profile")))
                  ))

       home-mime-service

       home-bind-utils-service

       home-direnv-service

       home-gdb-service

       home-ghci-service

       home-git-service
       home-gitconfig-service
       home-gita-service

       home-emacs-state-service
       home-emacs-service

       home-groovy-service

       home-gnupg-service

       (service home-gpg-agent-service-type
                (let ((ttl (* (* (* 60 60) 24) 30))) ;30 days
                  (home-gpg-agent-configuration
                   (pinentry-program
                    (file-append pinentry "/bin/pinentry"))
                   (default-cache-ttl ttl)
                   (max-cache-ttl ttl)
                   (default-cache-ttl-ssh ttl)
                   (max-cache-ttl-ssh ttl)
                   (ssh-support? #t)
                   (extra-content "\
pinentry-timeout 5
no-grab
allow-preset-passphrase"))))

       home-inputrc-service

       home-guile-service

       (simple-service 'keynav-config
                       home-files-service-type
                       (list `(".keynavrc" ,(local-file (string-append %distro-directory "/dot_keynavrc")))))

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
                       (list `(".Xmodmap" ,(local-file (string-append %distro-directory "/dot_Xmodmap")))))

       (simple-service 'xresources-config
                       home-files-service-type
                       (list `(".Xresources" ,(local-file (string-append %distro-directory "/dot_Xresources")))))

       home-qterminal-service

       (simple-service 'zathura-config
                       home-files-service-type
                       (list `(".config/zathura/zathurarc" ,(local-file (string-append %distro-directory "/dot_config/zathura/zathurarc")))))

       home-ripgrep-service

       home-gtk-service
       home-gtkrc-service

       (service home-alacritty-service-type)
       home-kitty-service

       (simple-service 'feh-config
                       home-files-service-type
                       (list `(".config/feh/buttons" ,(local-file (string-append %distro-directory "/dot_config/feh/buttons")))))

       (simple-service 'sway-config
                       home-files-service-type
                       (list `(".config/sway/config" ,(local-file (string-append %distro-directory "/dot_config/sway/config")))
                             `(".xkb/symbols/custom" ,(local-file (string-append %distro-directory "/dot_xkb/symbols/custom")))))

       (simple-service 'polybar-config
                       home-files-service-type
                       (list `(".config/polybar/config" ,(local-file (string-append %distro-directory "/dot_config/polybar/config")))))

       home-youtube-dl-service

       (simple-service 'cava-config
                       home-files-service-type
                       (list `(".config/cava/config" ,(local-file (string-append %distro-directory "/dot_config/cava/config")))))

       (simple-service 'termonad-config
                       home-files-service-type
                       (list `(".config/termonad/termonad.hs" ,(local-file (string-append %distro-directory "/dot_config/termonad/termonad.hs")))))

       home-nix-service

       home-mpv-service

       (simple-service 'cagebreak-config
                       home-files-service-type
                       (list `(".config/cagebreak/config" ,(local-file (string-append %distro-directory "/dot_config/cagebreak/config")))))

       (simple-service 'vis-config
                       home-files-service-type
                       (list `(".config/vis/config" ,(local-file (string-append %distro-directory "/dot_config/vis/config")))))

       (simple-service 'dunst-config
                       home-files-service-type
                       (list `(".config/dunst/dunstrc" ,(local-file (string-append %distro-directory "/dot_config/dunst/dunstrc")))))


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
                                             (list `("config/espanso/default.yml" ,#$(local-file (string-append %distro-directory "/dot_config/espanso/default.yml")))
                                                   ;; TODO: Add `("config/espanso/user/home.yml.tmpl" ,(local-file (string-append %distro-directory "/dot_config/espanso/user/home.yml.tmpl")))
                                                   `("config/espanso/user/systemd.yml" ,#$(local-file (string-append %distro-directory "/dot_config/espanso/user/systemd.yml")))
                                                   `("config/espanso/user/juniper.yml" ,#$(local-file (string-append %distro-directory "/dot_config/espanso/user/juniper.yml")))
                                                   `("config/espanso/user/mysql.yml" ,#$(local-file (string-append %distro-directory "/dot_config/espanso/user/mysql.yml")))
                                                   `("config/espanso/user/nix.yml" ,#$(local-file (string-append %distro-directory "/dot_config/espanso/user/nix.yml")))
                                                   `("config/espanso/user/mjru.yml" ,#$(local-file (string-append %distro-directory "/dot_config/espanso/user/mjru.yml"))))))))))

       (simple-service 'sshrc-config
                       home-files-service-type
                       (list `(".sshrc" ,(local-file (string-append %distro-directory "/dot_sshrc")))
                             `(".sshrc.d/.bashrc" ,(local-file (string-append %distro-directory "/dot_sshrc.d/dot_bashrc")))
                             `(".sshrc.d/.tmux.conf" ,(local-file (string-append %distro-directory "/dot_sshrc.d/dot_tmux.conf")))))

       (simple-service 'vnc-config
                       home-files-service-type
                       (list `(".vnc/default.tigervnc" ,(local-file (string-append %distro-directory "/private_dot_vnc/default.tigervnc")))
                             `(".vnc/xstartup" ,(local-file (string-append %distro-directory "/private_dot_vnc/xstartup") #:recursive? #t))
                             `(".vnc/xstartup-firefox" ,(local-file (string-append %distro-directory "/private_dot_vnc/xstartup-firefox") #:recursive? #t))
                             `(".vnc/xstartup-quassel" ,(local-file (string-append %distro-directory "/private_dot_vnc/xstartup-quassel") #:recursive? #t))
                             `(".vnc/xstartup-ratpoison" ,(local-file (string-append %distro-directory "/private_dot_vnc/xstartup-ratpoison") #:recursive? #t))
                             `(".vnc/xstartup-twm" ,(local-file (string-append %distro-directory "/private_dot_vnc/xstartup-twm") #:recursive? #t))))

       home-parallel-service

       home-msmtp-service

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
                                     (add-to-load-path (string-append %home "/src/cgit.wugi.info/wigust/guix/dotfiles"))
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
                       (append (list `(,".ansible.cfg" ,(local-file (string-append %distro-directory "/dot_ansible.cfg"))))
                               (map (lambda (file-name)
                                      `(,(string-append ".ansible/plugins/modules/" file-name)
                                        ,(local-file (string-append %distro-directory
                                                                    "/dot_ansible/plugins/modules/"
                                                                    file-name))))
                                    '("guix_package.py"
                                      "guix_pull.py"))))

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
                                    ("command" . "uptime")))))))))))))))))))
