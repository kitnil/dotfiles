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
             (gnu packages wm)

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

   (simple-service 'bin-config
                   home-files-service-type
                   (map (lambda (program)
                          `(,(string-append "local/bin/" program)
                            ,(local-file (string-append "dot_local/bin/executable_" program)
                                         #:recursive? #t)))
                        '("alerta-close"
                          "ansible-update-ssh-known-hosts"
                          "backup"
                          "bash-notify"
                          "block-ip"
                          "blog"
                          "brctl-start"
                          "cerb"
                          "checkssl.sh"
                          "chroot-games.sh"
                          "cisco"
                          "cisco-interact"
                          "clone-gitlab.intr.sh"
                          "color-converter"
                          "connect"
                          "convert-music"
                          "covid19"
                          "debian-chroot.sh"
                          "debian.sh"
                          "dns"
                          "dnscheck"
                          "domain.yml"
                          "dotfiles"
                          "elk-index-youtube"
                          "elogind-sway"
                          "emacs-guix-log"
                          "emacs-org-capture"
                          "emc"
                          "eww"
                          "fedora"
                          "ff"
                          "ffmpeg-hwaccel"
                          "ffmpeg-software"
                          "ffmpeg-youtube"
                          "fileshelter"
                          "firefox-guile"
                          "firefox-nix"
                          "firefox-youtube-chat"
                          "gita-dist"
                          "gita-kitnil"
                          "gita-mjru"
                          "gita-src"
                          "git-changelog-symlink-init.sh"
                          "git-gitlab"
                          "git-group-clone.sh"
                          "github-create-repository"
                          "gitlab-runner-service"
                          "git-mirror"
                          "git-pass-secrets"
                          "git-pure"
                          "git-statistics"
                          "gnus"
                          "godaddy"
                          "gpg-unlock"
                          "grafana"
                          "guile-git-list-commiters"
                          "guix-clean.sh"
                          "guix-custom-refresh"
                          "guix-environment.sh"
                          "guix-git-reset-to-current-channel"
                          "guix-latest"
                          "guix-my-services"
                          "guix-show"
                          "guix-update.sh"
                          "guix-weather-x86_64.sh"
                          "hms"
                          "import-cert.sh"
                          "iommu.sh"
                          "iproute2-bridge"
                          "jenkins"
                          "jenkins-active-jobs"
                          "jenkins-lastbuild"
                          "jenkins-local"
                          "jenkins-nix-version"
                          "lint"
                          "lists.sh"
                          "magit"
                          "mail"
                          "Majordomo_LLC_Root_CA.crt.sh"
                          "messages2notify-send"
                          "mj-hosts.sh"
                          "mjru-alerta"
                          "mjru-dns"
                          "mjru-docker"
                          "mjru-fetch-history"
                          "mjru-flake"
                          "mjru-git-clone.sh"
                          "mjru-github-projects.scm"
                          "mjru-grafana"
                          "mjru-infa"
                          "mjru-office"
                          "mjru-vpn.sh"
                          "monitor"
                          "monitoror"
                          "mpvctl"
                          "mpv-wrapper"
                          "my-docker"
                          "my-swank"
                          "my-xorg"
                          "nginx-server-name"
                          "nixos.sh"
                          "nix-repl"
                          "oracle"
                          "pers"
                          "prometheus-billing2"
                          "pulseaudio-switch-sink.sh"
                          "qemu-cdrom.sh"
                          "qemu-cdrom-vnc.sh"
                          "qemu-cdrom-win.sh"
                          "qemu-freebsd.sh"
                          "qemu-vpn.sh"
                          "record-video.sh"
                          "record-window"
                          "record-window-gif.sh"
                          "reevefresh"
                          "rofi-stumpwm"
                          "rofi-xterm"
                          "rss"
                          "run-docker"
                          "run-emacs"
                          "run-in-xterm"
                          "run-jenkins"
                          "run-jenkins-agent"
                          "run-kresd"
                          "run-nix-daemon"
                          "run-openvpn"
                          "run-place-existing-windows"
                          "run-stumpwm"
                          "sbcl"
                          "scan"
                          "shop"
                          "shutdown"
                          "src-clean"
                          "src-import.scm"
                          "ssh-aliases"
                          "ssh-sudo"
                          "ssh-vm"
                          "ssl"
                          "system"
                          "toggle-input-method.sh"
                          "tome4-docker"
                          "tranfser-curl"
                          "twitch.scm"
                          "ubuntu"
                          "vault"
                          "vfio.sh"
                          "video"
                          "vnc"
                          "vncview-5901.sh"
                          "volume-switch.sh"
                          "wallhaven"
                          "wallpaper"
                          "web-docker-pull"
                          "wi-emacs-shell.sh"
                          "wi-emacs-wget"
                          "wi-image-rotate.sh"
                          "wi-qemu-epson.sh"
                          "wi-show-colors"
                          "xclip-mpv.sh"
                          "xdg-open"
                          "xmenu.sh"
                          "xmodmap.sh"
                          "xterm-dark"
                          "yeastizzy"
                          "youtube-build"
                          "youtube-dl-json"
                          "youtube-dl-music-play-url"
                          "youtube-scm"
                          "yt")))

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
                                 `("bash_vterm" ,(local-file "../../dot_bash_vterm"))
                                 `("local/share/bash-completion/completions/lexicon" ,(local-file "../../dot_local/share/bash-completion/completions/lexicon"))
                                 `("local/share/bash-completion/completions/herd" ,(local-file "../../dot_local/share/bash-completion/completions/herd"))
                                 `("local/share/bash-completion/completions/mail" ,(local-file "../../dot_local/share/bash-completion/completions/mail"))
                                 `("local/share/bash-completion/completions/connect" ,(local-file "../../dot_local/share/bash-completion/completions/connect")))
                           (map (lambda (file-name)
                                  `(,(string-append "bash.d/" file-name) ,(local-file (string-append "dot_bash.d/" file-name))))
                                '("bash.scm"
                                  "mjru.bash"))))

   (simple-service 'applications
                   home-files-service-type
                   (list `("local/share/applications/mupdf.desktop" ,(local-file "../../dot_local/share/applications/mupdf.desktop"))
                         `("local/share/applications/gnus.desktop" ,(local-file "../../dot_local/share/applications/gnus.desktop"))
                         `("local/share/applications/org-protocol.desktop" ,(local-file "../../dot_local/share/applications/org-protocol.desktop"))
                         `("local/share/applications/mimeapps.list" ,(local-file "../../dot_local/share/applications/mimeapps.list"))
                         `("local/share/applications/guix-log.desktop" ,(local-file "../../dot_local/share/applications/guix-log.desktop"))
                         `("local/share/applications/feh.desktop" ,(local-file "../../dot_local/share/applications/feh.desktop"))))

   (simple-service 'direnv-config
                   home-files-service-type
                   (list `("direnvrc" ,(local-file "../../dot_direnvrc"))))

   (simple-service 'gdb-config
                   home-files-service-type
                   (list `("gdbinit" ,(local-file "../../dot_gdbinit"))))

   (simple-service 'ghci-config
                   home-files-service-type
                   (list `("ghci" ,(local-file "../../dot_ghci"))))

   (simple-service 'gitconfig-config
                   home-files-service-type
                   (list `("gitconfig" ,(local-file "../../dot_gitconfig"))))

   (simple-service 'emacs-config
                   home-files-service-type
                   (append (list `("emacs" ,(local-file "../../dot_emacs"))
                                 `("emacs.d/.mc-lists.el" ,(local-file "../../private_dot_emacs.d/dot_mc-lists.el"))
                                 `("gnus.el" ,(local-file "../../dot_gnus.el")))
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
                                  ;; TODO: "snippets/markdown-mode/support-timeout.tmpl"
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
                                  ;; TODO: "snippets/text-mode/hdd.tmpl"
                                  "snippets/conf-space-mode/mj"
                                  "snippets/message-mode/pushed-with-minor-changes"
                                  "snippets/message-mode/melpa"
                                  "snippets/message-mode/cgit-guix"
                                  "snippets/message-mode/push"
                                  "snippets/message-mode/proprietary"))
                           (map (lambda (file-name)
                                  `(,(string-append "emacs.d/" file-name) ,(local-file (string-append "private_dot_emacs.d/" file-name))))
                                '("insert/guix/gnu/services/service"
                                  "insert/guix/gnu/packages/package"
                                  "insert/guix/gnu/tests/test"
                                  "insert/guix/gnu/system/examples/vm-inherit-image"
                                  "insert/groovy/Jenkinsfile"
                                  "insert/guile/script"
                                  "insert/dotfiles/modules/services/service"
                                  "insert/nix/shell.nix"
                                  "insert/nix/flake.nix"))))

   (simple-service 'groovy-config
                   home-files-service-type
                   (list `("groovy/groovysh.rc" ,(local-file "../../dot_groovy/groovysh.rc"))))

   (simple-service 'gnupg-config
                   home-files-service-type
                   (map (lambda (file-name)
                          `(,(string-append "gnupg/" file-name) ,(local-file (string-append "private_dot_gnupg/" file-name))))
                        '("gpg-agent.conf"
                          "gpg.conf")))

   (simple-service 'inputrc-config
                   home-files-service-type
                   (list `("inputrc" ,(local-file "../../dot_inputrc"))))

   (simple-service 'guile-config
                   home-files-service-type
                   (list `("guile" ,(local-file "../../dot_guile"))))

   (simple-service 'keynav-config
                   home-files-service-type
                   (list `("keynavrc" ,(local-file "../../dot_keynavrc"))))

   (simple-service 'kodi-config
                   home-files-service-type
                   (list `("kodirc" ,(local-file "../../dot_kodirc"))))

   (simple-service 'mailcap-config
                   home-files-service-type
                   (list `("mailcap" ,(local-file "../../dot_mailcap"))))

   (simple-service 'mongo-config
                   home-files-service-type
                   (list `("mongorc.js" ,(local-file "../../dot_mongorc.js"))))

   (simple-service 'mycli-config
                   home-files-service-type
                   (list `("myclirc" ,(local-file "../../dot_myclirc"))))

   (simple-service 'nano-config
                   home-files-service-type
                   (list `("nanorc" ,(local-file "../../dot_nanorc"))))

   (simple-service 'python-config
                   home-files-service-type
                   (list `("pythonrc" ,(local-file "../../dot_pythonrc"))))

   (simple-service 'sbcl-config
                   home-files-service-type
                   (list `("sbcl_completions" ,(local-file "../../dot_sbcl_completions"))))

   (simple-service 'screen-config
                   home-files-service-type
                   (list `("screenrc" ,(local-file "../../dot_screenrc"))))

   (simple-service 'tmux-config
                   home-files-service-type
                   (list `("tmux.conf" ,(local-file "../../dot_tmux.conf"))))

   (simple-service 'tmuxifier-config
                   home-files-service-type
                   (list `("tmuxifier-layouts/backup.session.sh" ,(local-file "../../dot_tmuxifier-layouts/backup.session.sh"))
                         `("tmuxifier-layouts/backup.window.sh" ,(local-file "../../dot_tmuxifier-layouts/backup.window.sh"))
                         `("tmuxifier-layouts/blog.session.sh" ,(local-file "../../dot_tmuxifier-layouts/blog.session.sh"))
                         `("tmuxifier-layouts/blog.window.sh" ,(local-file "../../dot_tmuxifier-layouts/blog.window.sh"))
                         `("tmuxifier-layouts/guix-machines.window.sh" ,(local-file "../../dot_tmuxifier-layouts/guix-machines.window.sh"))
                         `("tmuxifier-layouts/guix.session.sh" ,(local-file "../../dot_tmuxifier-layouts/guix.session.sh"))
                         `("tmuxifier-layouts/guix.window.sh" ,(local-file "../../dot_tmuxifier-layouts/guix.window.sh"))
                         ;; TODO: `("web.session.sh.tmpl" ,(local-file "../../dot_tmuxifier-layouts/web.session.sh.tmpl"))
                         ))

   (simple-service 'top-config
                   home-files-service-type
                   (list `("toprc" ,(local-file "../../dot_toprc"))))

   (simple-service 'xmodmap-config
                   home-files-service-type
                   (list `("Xmodmap" ,(local-file "../../dot_Xmodmap"))))

   (simple-service 'xresources-config
                   home-files-service-type
                   (list `("Xresources" ,(local-file "../../dot_Xresources"))))

   (simple-service 'git-config
                   home-files-service-type
                   (list `("config/git/gitk" ,(local-file "../../dot_config/git/gitk"))
                         `("config/git/ignore" ,(local-file "../../dot_config/git/ignore"))))

   (simple-service 'qterminal-config
                   home-files-service-type
                   (list `("config/qterminal.org/qterminal.ini" ,(local-file "../../dot_config/qterminal.org/qterminal.ini"))
                         `("config/qterminal.org/qterminal_bookmarks.xml" ,(local-file "../../dot_config/qterminal.org/qterminal_bookmarks.xml"))))

   (simple-service 'zathura-config
                   home-files-service-type
                   (list `("config/zathura/zathurarc" ,(local-file "../../dot_config/zathura/zathurarc"))))

   (simple-service 'ripgrep-config
                   home-files-service-type
                   (list `("config/ripgrep/ripgreprc" ,(local-file "../../dot_config/ripgrep/ripgreprc"))))

   (simple-service 'gtk-config
                   home-files-service-type
                   (list `("config/gtk-3.0/gtk.css" ,(local-file "../../dot_config/gtk-3.0/gtk.css"))
                         `("config/gtk-3.0/settings.ini" ,(local-file "../../dot_config/gtk-3.0/settings.ini"))))

   (simple-service 'greenclip-config
                   home-files-service-type
                   (list `("config/greenclip.cfg" ,(local-file "../../dot_config/greenclip.cfg"))))

   (simple-service 'alacritty-config
                   home-files-service-type
                   (list `("config/alacritty/themes/xterm.yml" ,(local-file "../../dot_config/alacritty/themes/xterm.yml"))
                         `("config/alacritty/alacritty.yml" ,(local-file "../../dot_config/alacritty/alacritty.yml"))))

   (simple-service 'feh-config
                   home-files-service-type
                   (list `("config/feh/buttons" ,(local-file "../../dot_config/feh/buttons"))))

   (simple-service 'sway-config
                   home-files-service-type
                   (list `("config/sway/config" ,(local-file "../../dot_config/sway/config"))))

   (simple-service 'polybar-config
                   home-files-service-type
                   (list `("config/polybar/config" ,(local-file "../../dot_config/polybar/config"))))

   (simple-service 'htop-config
                   home-files-service-type
                   (list `("config/htop/htoprc" ,(local-file "../../dot_config/htop/htoprc"))))

   (simple-service 'transmission-config
                   home-files-service-type
                   (list `("config/transmission/settings.json" ,(local-file "../../dot_config/transmission/settings.json"))))

   (simple-service 'kitty-config
                   home-files-service-type
                   (list `("config/kitty/kitty.conf" ,(local-file "../../dot_config/kitty/kitty.conf"))))

   (simple-service 'gita-config
                   home-files-service-type
                   (list `("config/gita/cmds.yml" ,(local-file "../../dot_config/gita/cmds.yml"))))

   (simple-service 'youtube-dl-config
                   home-files-service-type
                   (list `("config/youtube-dl/config" ,(local-file "../../dot_config/youtube-dl/config"))))

   (simple-service 'postgresql-config
                   home-files-service-type
                   (list `("config/autopostgresqlbackup.conf" ,(local-file "../../dot_config/autopostgresqlbackup.conf"))))

   (simple-service 'cava-config
                   home-files-service-type
                   (list `("config/cava/config" ,(local-file "../../dot_config/cava/config"))))

   (simple-service 'termonad-config
                   home-files-service-type
                   (list `("config/termonad/termonad.hs" ,(local-file "../../dot_config/termonad/termonad.hs"))))

   (simple-service 'nix-config
                   home-files-service-type
                   (list `("config/nix/repl.nix" ,(local-file "../../dot_config/nix/repl.nix"))
                         `("config/nix/nix.conf" ,(local-file "../../dot_config/nix/nix.conf"))
                         `("config/nix/registry.json" ,(local-file "../../dot_config/nix/registry.json"))
                         `("config/nixpkgs/config.nix" ,(local-file "../../dot_config/nixpkgs/config.nix"))))

   (simple-service 'mpv-config
                   home-files-service-type
                   (list `("config/mpv/input.conf" ,(local-file "../../dot_config/mpv/input.conf"))
                         `("config/mpv/mpv.conf" ,(local-file "../../dot_config/mpv/mpv.conf"))))

   (simple-service 'cagebreak-config
                   home-files-service-type
                   (list `("config/cagebreak/config" ,(local-file "../../dot_config/cagebreak/config"))))

   (simple-service 'vis-config
                   home-files-service-type
                   (list `("config/vis/config" ,(local-file "../../dot_config/vis/config"))))

   (simple-service 'dunst-config
                   home-files-service-type
                   (list `("config/dunst/dunstrc.tmpl" ,(local-file "../../dot_config/dunst/dunstrc.tmpl"))))


   ;; TODO: Add those
   ;; dot_config/guile/mariadb.scm
   ;; dot_config/mjru/encrypted_config.scm
   ;; dot_config/mjru/encrypted_firefox.scm

   (simple-service 'espanso-config
                   home-files-service-type
                   (list `("config/espanso/default.yml" ,(local-file "../../dot_config/espanso/default.yml"))
                         ;; TODO: Add `("config/espanso/user/home.yml.tmpl" ,(local-file "../../dot_config/espanso/user/home.yml.tmpl"))
                         `("config/espanso/user/systemd.yml" ,(local-file "../../dot_config/espanso/user/systemd.yml"))
                         `("config/espanso/user/juniper.yml" ,(local-file "../../dot_config/espanso/user/juniper.yml"))
                         `("config/espanso/user/mysql.yml" ,(local-file "../../dot_config/espanso/user/mysql.yml"))
                         `("config/espanso/user/nix.yml" ,(local-file "../../dot_config/espanso/user/nix.yml"))
                         ;; TODO: Add dot_config/espanso/user/mjru.yml.tmpl
                         ))

   (simple-service 'ssh-config
                   home-activation-service-type
                   #~(begin
                       (add-to-load-path (string-append #$%home "/.local/share/chezmoi/dotfiles"))
                       (use-modules (ice-9 rdelim)
                                    (ice-9 popen)
                                    (guile gpg))
                       (let ((ssh (string-append #$%home "/.ssh")))
                         (unless (file-exists? ssh)
                           (mkdir ssh))
                         (chmod ssh #o700)
                         (gpg->file #$(local-file "../../private_dot_ssh/encrypted_private_known_hosts")
                                    (string-append ssh "/known_hosts"))
                         (gpg->file #$(local-file "../../private_dot_ssh/encrypted_private_authorized_keys")
                                    (string-append ssh "/authorized_keys")))))

   (simple-service 'xsession-config
                   home-files-service-type
                   (list
                    `("xsession"
                      ,(let ((stumpwp-load-file
                              (plain-file
                               "stumpwp-load-file"
                               (with-output-to-string
                                 (lambda ()
                                   (display '(require :asdf))
                                   (newline)
                                   (display '(require :stumpwm))
                                   (newline)
                                   (display '(stumpwm:stumpwm))
                                   (newline))))))
                         (computed-file
                          "xsession"
                          #~(begin
                              (call-with-output-file #$output
                                (lambda (port)
                                  (display "#!/bin/sh\n" port)
                                  (format port "~a -r ~a &~%"
                                          #$(file-append xclickroot "/bin/xclickroot")
                                          #$(local-file "../../dot_local/bin/executable_xmenu.sh"))
                                  (format port "exec -a stumpwm /run/current-system/profile/bin/sbcl --load ~a~%"
                                          #$stumpwp-load-file)))
                              (chmod #$output #o555)))))))

   (simple-service 'parallel-config
                   home-activation-service-type
                   #~(begin
                       (let* ((%home
                               (and=> (getenv "HOME")
                                      (lambda (home)
                                        home)))
                              (parallel (string-append %home "/.parallel")))
                         (unless (file-exists? parallel)
                           (mkdir parallel))
                         (call-with-output-file (string-append parallel "/runs-without-willing-to-cite")
                           (lambda (port)
                             (display "6\n" port))))))

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
                       (call-with-output-file (string-append %home "/.msmtprc")
                         (lambda (port)
                           (format port "\
# Set default values for all following accounts.
defaults
auth           on
tls            on
tls_trust_file /etc/ssl/certs/ca-certificates.crt
logfile        ~/.msmtp.log

# Gmail
account        gmail
host           smtp.gmail.com
port           587
from           go.wigust@gmail.com
user           go.wigust
password       ~a

# Set a default account
account default : gmail
"
                                   (pass "email/gmail/go.wigust"))))))

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
                                     %home))))))

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
