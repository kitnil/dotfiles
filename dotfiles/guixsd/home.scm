(use-modules (gnu home)
             (gnu home services)
             ;; (gnu home services files)
             (gnu home services mcron)
             (gnu home services shells)
             ;; (gnu home services ssh)
             (gnu packages admin)
             (gnu packages guile)
             (gnu services)
             (guix gexp)
             (guix modules)
             (guix profiles)
             (ice-9 rdelim)
             (json)

             (gnu packages haskell-apps)

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

   (service home-mcron-service-type)
   (service nix-delete-generations-service-type
            (nix-delete-generations-configuration
             (schedule '(next-hour '(21)))))
   (service guix-delete-generations-service-type
            (guix-delete-generations-configuration
             (schedule '(next-hour '(21)))
             (period "1m")))
   
   ;; XXX: missing home-ssh-configuration
   ;; (service home-ssh-service-type
   ;;          (home-ssh-configuration
   ;;           (extra-config
   ;;            (list
   ;;             (ssh-host "savannah"
   ;;      		 '((compression . #f)))))))

   (service home-greenclip-service-type)

   )))
