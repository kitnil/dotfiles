(use-modules (gnu home)
             (gnu home services)
             ;; (gnu home services files)
             (gnu home services mcron)
             (gnu home services shells)
             ;; (gnu home services ssh)
             (gnu packages admin)
             (gnu packages guile)
             (gnu services)
             (gnu services configuration)
             (guix gexp)
             (guix modules)
             (guix profiles)
             (ice-9 rdelim)
             (json)

             (gnu packages haskell-apps)

             (home services ansible)
             (home services desktop)
             (home services mail)
             (home services package-management)
             (gnu packages mail)
             (guile pass)
             (packages virtualization))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define .bash_profile
  (string-append %home "/.local/share/chezmoi/dot_bash_profile"))

(define .bashrc
  (string-append %home "/.local/share/chezmoi/dot_bashrc"))

(define mbsync-gmail
  (mbsync-config-file
   (imap-account "gmail")
   (host "imap.gmail.com")
   (user "go.wigust@gmail.com")
   (pass-cmd "gpg -q --for-your-eyes-only --no-tty -d ~/.password-store/email/gmail/go.wigust.gpg")
   (auth-mechs "LOGIN")
   (ssl-type "IMAPS")
   (certificate-file "/etc/ssl/certs/ca-certificates.crt")
   (pipeline-depth "50")
   (imap-store "gmail-remote")
   (account "gmail")
   (maildir-store "gmail-local")
   (path "~/Maildir/")
   (inbox "~/Maildir/INBOX")
   (sub-folders "Verbatim")
   (group "gmail-all")
   (channel "gmail")
   (far ":gmail-remote:")
   (near ":gmail-local:")
   (patterns "INBOX") ;Sync only "INBOX"
   (max-messages "2000")
   (expunge "near")
   (sync "Pull")))

(define mbsync-majordomo
  (mbsync-config-file
   (imap-account "majordomo")
   (host "router.majordomo.ru")
   (user "pyhalov@majordomo.ru")
   (pass-cmd "pass show majordomo/private/newmail.majordomo.ru/pyhalov@majordomo.ru")
   (auth-mechs "LOGIN")
   (ssl-type "None")
   (certificate-file "/etc/ssl/certs/ca-certificates.crt")
   (pipeline-depth "50")
   (imap-store "majordomo-remote")
   (account "majordomo")
   (maildir-store "majordomo-local")
   (path "~/Maildir/")
   (inbox "~/Maildir/majordomo")
   (sub-folders "Verbatim")
   (channel "majordomo")
   (far ":majordomo-remote:")
   (near ":majordomo-local:")
   (patterns "INBOX") ;Sync only "INBOX"
   (sync "Pull")))

(define mbsync-majordomo-sidorov
  (mbsync-config-file
   (imap-account "majordomo-sidorov")
   (host "router.majordomo.ru")
   (user "sidorov@majordomo.ru")
   (pass-cmd "pass show majordomo/private/router.majordomo.ru/sidorov@majordomo.ru")
   (auth-mechs "LOGIN")
   (ssl-type "None")
   (certificate-file "/etc/ssl/certs/ca-certificates.crt")
   (pipeline-depth "50")
   (imap-store "majordomo-sidorov-remote")
   (account "majordomo-sidorov")
   (maildir-store "majordomo-sidorov-local")
   (path "~/Maildir/")
   (inbox "~/Maildir/majordomo-sidorov")
   (sub-folders "Verbatim")
   (channel "majordomo-sidorov")
   (far ":majordomo-sidorov-remote:")
   (near ":majordomo-sidorov-local:")
   (patterns "INBOX") ;Sync only "INBOX"
   (sync "Pull")))

(define mbsync-majordomo-healthchecks
  (mbsync-config-file
   (imap-account "majordomo-healthchecks")
   (host "router.majordomo.ru")
   (user "healthchecks@majordomo.ru")
   (pass-cmd "pass show majordomo/private/router.majordomo.ru/healthchecks@majordomo.ru")
   (auth-mechs "LOGIN")
   (ssl-type "None")
   (certificate-file "/etc/ssl/certs/ca-certificates.crt")
   (pipeline-depth "50")
   (imap-store "majordomo-healthchecks-remote")
   (account "majordomo-healthchecks")
   (maildir-store "majordomo-healthchecks-local")
   (path "~/Maildir/")
   (inbox "~/Maildir/majordomo-healthchecks")
   (sub-folders "Verbatim")
   (channel "majordomo-healthchecks")
   (far ":majordomo-healthchecks-remote:")
   (near ":majordomo-healthchecks-local:")
   (patterns "INBOX") ;Sync only "INBOX"
   (sync "Pull")))

(define mbsync-wugi
  (mbsync-config-file
   (imap-account "wugi")
   (host "smtp.wugi.info")
   (user "oleg@smtp.wugi.info")
   (pass-cmd "gpg -q --for-your-eyes-only --no-tty -d ~/.password-store/localhost/imap/oleg.gpg")
   (auth-mechs "LOGIN")
   (ssl-type "IMAPS")
   (certificate-file "/etc/ssl/certs/ca-certificates.crt")
   (pipeline-depth "50")
   (imap-store "wugi-remote")
   (account "wugi")
   (maildir-store "wugi-local")
   (path "~/Maildir/")
   (inbox "~/Maildir/wugi.info")
   (sub-folders "Verbatim")
   (channel "wugi")
   (far ":wugi-remote:")
   (near ":wugi-local:")
   (patterns "INBOX") ;Sync only "INBOX"
   (sync "Pull")))

(home-environment

 (packages (map manifest-entry-item
                (manifest-entries
                 (load "../manifests/guixsd.scm"))))

 (services
  (list

   (service home-goimapnotify-service-type
            (goimapnotify-configuration
             (config-file
              (computed-file
               "isync-gmail-config"
               (with-extensions (list guile-json-4)
                 (with-imported-modules (source-module-closure '((json builder)))
                   #~(begin
                       (use-modules (json builder))
                       (define isync
                         #$(file-append isync "/bin/mbsync"))
                       (define password
                         #$(pass "show" "email/gmail/go.wigust"))
                       (with-output-to-file #$output
                         (lambda ()
                           (scm->json
                            `(("boxes" . #("INBOX"))
                              ("onNewMail" . ,(string-join (list isync "gmail")))
                              ("xoauth2" . #f)
                              ("password" . ,password)
                              ("username" . "go.wigust@gmail.com")
                              ("tlsOptions" ("rejectUnauthorized" . #t))
                              ("tls" . #t)
                              ("port" . 993)
                              ("host" . "imap.gmail.com"))
                            #:pretty #t))))))))))

   (service home-goimapnotify-service-type
            (goimapnotify-configuration
             (config-file
              (computed-file
               "isync-majordomo-config"
               (with-extensions (list guile-json-4)
                 (with-imported-modules (source-module-closure '((json builder)))
                   #~(begin
                       (use-modules (json builder))
                       (define isync
                         #$(file-append isync "/bin/mbsync"))
                       (define password
                         #$(pass "show" "majordomo/private/newmail.majordomo.ru/pyhalov@majordomo.ru"))
                       (with-output-to-file #$output
                         (lambda ()
                           (scm->json
                            `(("boxes" . #("INBOX"))
                              ("onNewMail" . ,(string-join (list isync "majordomo")))
                              ("xoauth2" . #f)
                              ("password" . ,password)
                              ("username" . "pyhalov@majordomo.ru")
                              ("tlsOptions" ("rejectUnauthorized" . #t))
                              ("tls" . #t)
                              ("port" . 993)
                              ("host" . "imap.majordomo.ru"))
                            #:pretty #t))))))))))

   (service home-goimapnotify-service-type
            (goimapnotify-configuration
             (config-file
              (computed-file
               "isync-wugi-config"
               (with-extensions (list guile-json-4)
                 (with-imported-modules (source-module-closure '((json builder)))
                   #~(begin
                       (use-modules (json builder))
                       (define isync
                         #$(file-append isync "/bin/mbsync"))
                       (define password
                         #$(pass "show" "localhost/imap/oleg"))
                       (with-output-to-file #$output
                         (lambda ()
                           (scm->json
                            `(("boxes" . #("INBOX"))
                              ("onNewMail" . ,(string-join (list isync "wugi")))
                              ("xoauth2" . #f)
                              ("password" . ,password)
                              ("username" . "oleg@wugi.info")
                              ("tlsOptions" ("rejectUnauthorized" . #t))
                              ("tls" . #t)
                              ("port" . 993)
                              ("host" . "smtp.wugi.info"))
                            #:pretty #t))))))))))

   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bashrc
              (list
               (local-file .bashrc)))
             (bash-profile
              (list
               (local-file .bash_profile)))))

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

   (simple-service 'home-mbsync-config
                   home-files-service-type
                   (list `("mbsyncrc"
                           ,(computed-file "mbsyncrc"
                                           #~(begin
                                               (with-output-to-file #$output
                                                 (lambda ()
                                                   (display #$(serialize-text-config
                                                               #f
                                                               (list mbsync-gmail
                                                                     mbsync-majordomo
                                                                     mbsync-majordomo-sidorov
                                                                     mbsync-majordomo-healthchecks
                                                                     mbsync-wugi))))))))))

   (simple-service 'chromium-wrapper
                   home-files-service-type
                   (map (lambda (program)
                          `(,(string-append "local/bin/" program)
                            ,(computed-file
                              program
                              #~(begin
                                  (with-output-to-file #$output
                                    (lambda ()
                                      (format #t "\
#!/bin/sh
# https://github.com/stumpwm/stumpwm/issues/894
exec -a \"$0\" /home/oleg/.nix-profile/bin/~a --disable-features=SendMouseLeaveEvents \"$@\"\n"
                                              #$program)))
                                  (chmod #$output #o555)))))
                        '("google-chrome-stable" "chromium")))

   (simple-service 'looking-glass-wrapper
                   home-files-service-type
                   (list `("local/bin/looking-glass-wrapper"
                           ,(program-file "looking-glass-wrapper"
                                          #~(system* #$(file-append looking-glass-client-next "/bin/looking-glass-client")
                                                     "-F"
                                                     "spice:enable" "no"
                                                     "wayland:warpSupport" "no"
                                                     "input:grabKeyboard" "no"
                                                     "win:dontUpscale" "yes")))))

   (simple-service 'shellcheck-wrapper
                   home-files-service-type
                   (list `("local/bin/shellcheck"
                           ,(computed-file
                             "shellcheck-wrapper"
                             #~(begin
                                 (with-output-to-file #$output
                                   (lambda ()
                                     (format #t "\
#!/bin/sh
exec -a \"$0\" ~a/bin/shellcheck --shell=bash \"$@\"\n"
                                             #$shellcheck)))
                                 (chmod #$output #o555))))))

   (simple-service 'stumpwm-config
                   home-files-service-type
                   (map (lambda (file-name)
                          `(,(string-append "stumpwm.d/" file-name) ,(local-file (string-append "dot_stumpwm.d/" file-name))))
                        '("admin.lisp"
                          "android.lisp"
                          "audio.lisp"
                          "autostart.lisp"
                          "backup.lisp"
                          "chat.lisp"
                          "clipboard.lisp"
                          "covid19.lisp"
                          "cpu.lisp"
                          "desktop-0.lisp"
                          "disk.lisp"
                          "display-0.lisp"
                          "display-1.lisp"
                          "docker.lisp"
                          "documentation.lisp"
                          "emacs.lisp"
                          "gaps.lisp"
                          "gpg.lisp"
                          "group-1.lisp"
                          "hardware.lisp"
                          "imap.lisp"
                          "init.lisp"
                          "keys.lisp"
                          "kodi.lisp"
                          "kvm.lisp"
                          "mail.lisp"
                          "mem.lisp"
                          "mjru.lisp"
                          "mode-line.lisp"
                          "mpv.lisp"
                          "nav.lisp"
                          "notify.lisp"
                          "password.lisp"
                          "repl.lisp"
                          "rest.lisp"
                          "rofi.lisp"
                          "screenshoot.lisp"
                          "spb.lisp"
                          "streamlink.lisp"
                          "swank.lisp"
                          "term.lisp"
                          "text-editors.lisp"
                          "theme.lisp"
                          "time.lisp"
                          "torrent.lisp"
                          "trans.lisp"
                          "utils.lisp"
                          "vm.lisp"
                          "vnc.lisp"
                          "web.lisp"
                          "xorg.lisp"
                          "youtube-dl.lisp")))

   (simple-service 'bash-config
                   home-files-service-type
                   (append (list `("bash_completion" ,(local-file "../../dot_bash_completion"))
                                 `("bash_guix" ,(local-file "../../dot_bash_guix"))
                                 `("bash_vterm" ,(local-file "../../dot_bash_vterm")))
                           (map (lambda (file-name)
                                  `(,(string-append "bash.d/" file-name) ,(local-file (string-append "dot_bash.d/" file-name))))
                                '("bash.scm"
                                  "mjru.bash"))))

   (service home-mcron-service-type)
   (service nix-delete-generations-service-type
            (nix-delete-generations-configuration
             (schedule '(next-hour '(21)))))
   (service guix-delete-generations-service-type
            (guix-delete-generations-configuration
             (schedule '(next-hour '(21)))
             (period "1m")))

   (service ansible-playbook-service-type)
   (simple-service 'ansible-config
                   home-files-service-type
                   (append (list `(,"ansible.cfg" ,(local-file "../../dot_ansible.cfg")))
                           (map (lambda (file-name)
                                  `(,(string-append "ansible/plugins/modules/" file-name) ,(local-file (string-append "dot_ansible/plugins/modules/" file-name))))
                                '("guix_package.py"
                                  "guix_pull.py"))))

   
   ;; XXX: missing home-ssh-configuration
   ;; (service home-ssh-service-type
   ;;          (home-ssh-configuration
   ;;           (extra-config
   ;;            (list
   ;;             (ssh-host "savannah"
   ;;      		 '((compression . #f)))))))

   (service home-greenclip-service-type)

   )))
