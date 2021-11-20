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

   (simple-service 'direnv-config
                   home-files-service-type
                   (list `("direnvrc" ,(local-file "../../dot_direnvrc"))))

   (simple-service 'emacs-config
                   home-files-service-type
                   (append (list `("emacs" ,(local-file "../../dot_emacs"))
                                 `("emacs.d/.mc-lists.el" ,(local-file "../../private_dot_emacs.d/dot_mc-lists.el")))
                           (map (lambda (file-name)
                                  `(,(string-append "emacs.d/" file-name) ,(local-file (string-append "private_dot_emacs.d/" file-name))))
                                '("abbrev_defs"
                                  "org-generate.org"
                                  "modules/audio.el"
                                  "modules/blog.el"
                                  "modules/c.el"
                                  "modules/ci.el"
                                  "modules/compile.el"
                                  "modules/completion.el"
                                  "modules/copyright.el"
                                  "modules/debbugs.el"
                                  "modules/debug.el"
                                  "modules/dired.el"
                                  "modules/elfeed.el"
                                  "modules/erc.el"
                                  "modules/ffap.el"
                                  "modules/files.el"
                                  "modules/ftp.el"
                                  "modules/groovy.el"
                                  "modules/guix.el"
                                  "modules/haskell.el"
                                  "modules/hooks.el"
                                  "modules/info.el"
                                  "modules/java.el"
                                  "modules/keys.el"
                                  "modules/lisp.el"
                                  "modules/lsp.el"
                                  "modules/mail.el"
                                  "modules/majordomo.el"
                                  "modules/ml.el"
                                  "modules/nav.el"
                                  "modules/nix.el"
                                  "modules/org.el"
                                  "modules/outline.el"
                                  "modules/perl.el"
                                  "modules/po.el"
                                  "modules/python.el"
                                  "modules/rfc.el"
                                  "modules/rust.el"
                                  "modules/scheme.el"
                                  "modules/slack.el"
                                  "modules/snippets.el"
                                  "modules/term.el"
                                  "modules/text.el"
                                  "modules/theme.el"
                                  "modules/time.el"
                                  "modules/tramp.el"
                                  "modules/twitch.el"
                                  "modules/utils.el"
                                  "modules/version-control.el"
                                  "modules/version-control-lexical.el"
                                  "modules/web.el"
                                  "modules/youtube.el"

                                  "snippets/erc-mode/problem"
                                  "snippets/markdown-mode/ssl-connect"
                                  "snippets/markdown-mode/not-available-from-network"
                                  "snippets/markdown-mode/law-vps-without-admin"
                                  "snippets/markdown-mode/support-timeout.tmpl"
                                  "snippets/markdown-mode/wrong-control-panel"
                                  "snippets/markdown-mode/additional-request"
                                  "snippets/markdown-mode/dns-more-time"
                                  "snippets/markdown-mode/upload"
                                  "snippets/markdown-mode/archive-extract"
                                  "snippets/markdown-mode/mail-mail-ru"
                                  "snippets/geiser-repl-mode/module-set"
                                  "snippets/apache-mode/vhost"
                                  "snippets/apache-mode/vhost-bitrix"
                                  "snippets/shell-mode/guix-search"
                                  "snippets/shell-mode/guix-system-reconfigure"
                                  "snippets/shell-mode/guix-environment-guix"
                                  "snippets/shell-mode/guix-configure"
                                  "snippets/shell-mode/guix-weather-manifest"
                                  "snippets/shell-mode/guix-package-manifest"
                                  "snippets/shell-mode/guix-wigust"
                                  "snippets/shell-mode/guix-graph"
                                  "snippets/shell-mode/guix-system-link"
                                  "snippets/scheme-mode/package-emacs-git"
                                  "snippets/scheme-mode/service-config-entry"
                                  "snippets/scheme-mode/pretty-print"
                                  "snippets/scheme-mode/system-stdout"
                                  "snippets/scheme-mode/letvar"
                                  "snippets/scheme-mode/list-comprehension"
                                  "snippets/scheme-mode/package"
                                  "snippets/scheme-mode/git-checkout"
                                  "snippets/scheme-mode/let-pretty-print"
                                  "snippets/scheme-mode/define-record-type"
                                  "snippets/terraform-mode/ssh-sup-service"
                                  "snippets/terraform-mode/ssh-sup-room"
                                  "snippets/terraform-mode/majordomo-gitlab-user"
                                  "snippets/nginx-mode/nginx-redirect"
                                  "snippets/python-mode/ansible-module"
                                  "snippets/python-mode/click"
                                  "snippets/lisp-mode/map-top"
                                  "snippets/lisp-mode/thread"
                                  "snippets/lisp-mode/command"
                                  "snippets/nix-mode/mj-overlay"
                                  "snippets/nix-mode/test"
                                  "snippets/nix-mode/optional"
                                  "snippets/nix-mode/vm-xfce"
                                  "snippets/nix-mode/pp"
                                  "snippets/snippets/erc-mode/problem"
                                  "snippets/snippets/groovy-mode/parallel"
                                  "snippets/snippets/groovy-mode/shared"
                                  "snippets/snippets/markdown-mode/ssl-connect"
                                  "snippets/snippets/markdown-mode/not-available-from-network"
                                  "snippets/snippets/markdown-mode/law-vps-without-admin"
                                  "snippets/snippets/markdown-mode/wrong-control-panel"
                                  "snippets/snippets/markdown-mode/additional-request"
                                  "snippets/snippets/markdown-mode/dns-more-time"
                                  "snippets/snippets/markdown-mode/upload"
                                  "snippets/snippets/markdown-mode/archive-extract"
                                  "snippets/snippets/markdown-mode/mail-mail-ru"
                                  "snippets/snippets/geiser-repl-mode/module-set"
                                  "snippets/snippets/apache-mode/vhost"
                                  "snippets/snippets/apache-mode/vhost-bitrix"
                                  "snippets/snippets/shell-mode/guix-search"
                                  "snippets/snippets/shell-mode/guix-system-reconfigure"
                                  "snippets/snippets/shell-mode/guix-environment-guix"
                                  "snippets/snippets/shell-mode/guix-configure"
                                  "snippets/snippets/shell-mode/guix-weather-manifest"
                                  "snippets/snippets/shell-mode/guix-package-manifest"
                                  "snippets/snippets/shell-mode/guix-wigust"
                                  "snippets/snippets/shell-mode/guix-graph"
                                  "snippets/snippets/shell-mode/guix-system-link"
                                  "snippets/snippets/scheme-mode/package-emacs-git"
                                  "snippets/snippets/scheme-mode/service-config-entry"
                                  "snippets/snippets/scheme-mode/pretty-print"
                                  "snippets/snippets/scheme-mode/system-stdout"
                                  "snippets/snippets/scheme-mode/letvar"
                                  "snippets/snippets/scheme-mode/list-comprehension"
                                  "snippets/snippets/scheme-mode/package"
                                  "snippets/snippets/scheme-mode/git-checkout"
                                  "snippets/snippets/scheme-mode/let-pretty-print"
                                  "snippets/snippets/scheme-mode/define-record-type"
                                  "snippets/snippets/terraform-mode/ssh-sup-service"
                                  "snippets/snippets/terraform-mode/ssh-sup-room"
                                  "snippets/snippets/terraform-mode/majordomo-gitlab-user"
                                  "snippets/snippets/nginx-mode/nginx-redirect"
                                  "snippets/snippets/python-mode/click"
                                  "snippets/snippets/php-mode/mail"
                                  "snippets/snippets/lisp-mode/map-top"
                                  "snippets/snippets/lisp-mode/command"
                                  "snippets/snippets/nix-mode/mj-overlay"
                                  "snippets/snippets/nix-mode/test"
                                  "snippets/snippets/nix-mode/optional"
                                  "snippets/snippets/nix-mode/vm-xfce"
                                  "snippets/snippets/nix-mode/pp"
                                  "snippets/snippets/text-mode/web-control-auth"
                                  "snippets/snippets/text-mode/init"
                                  "snippets/snippets/text-mode/dot"
                                  "snippets/snippets/text-mode/web-is-not-available"
                                  "snippets/snippets/text-mode/web-ftp-passwd"
                                  "snippets/snippets/text-mode/subject-account"
                                  "snippets/snippets/text-mode/ftp-passwd"
                                  "snippets/snippets/text-mode/start"
                                  "snippets/snippets/conf-space-mode/mj"
                                  "snippets/snippets/message-mode/pushed-with-minor-changes"
                                  "snippets/snippets/message-mode/melpa"
                                  "snippets/snippets/message-mode/cgit-guix"
                                  "snippets/snippets/message-mode/push"
                                  "snippets/text-mode/web-control-auth"
                                  "snippets/text-mode/init"
                                  "snippets/text-mode/dot"
                                  "snippets/text-mode/web-is-not-available"
                                  "snippets/text-mode/web-ftp-passwd"
                                  "snippets/text-mode/subject-account"
                                  "snippets/text-mode/ftp-passwd"
                                  "snippets/text-mode/start"
                                  "snippets/text-mode/hdd.tmpl"
                                  "snippets/conf-space-mode/mj"
                                  "snippets/message-mode/pushed-with-minor-changes"
                                  "snippets/message-mode/melpa"
                                  "snippets/message-mode/cgit-guix"
                                  "snippets/message-mode/push"
                                  "snippets/message-mode/proprietary"))))

   (simple-service 'gnupg-config
                   home-files-service-type
                   (map (lambda (file-name)
                          `(,(string-append "gnupg/" file-name) ,(local-file (string-append "private_dot_gnupg/" file-name))))
                        '("gpg-agent.conf"
                          "gpg.conf")))

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
