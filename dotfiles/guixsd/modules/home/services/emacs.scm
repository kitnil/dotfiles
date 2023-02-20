(define-module (home services emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:export (home-emacs-service
            home-emacs-state-service))

(define home-emacs-state-service
  (simple-service 'emacs-state
                  home-activation-service-type
                  #~(invoke
                     #$(program-file
                        "emacs-state"
                        (with-imported-modules '((ice-9 match))
                          #~(begin
                              (use-modules (ice-9 match))
                              (for-each
                               (match-lambda
                                 ((destination source)
                                  (let ((destination-full-path
                                         (string-append
                                          #$%home "/." destination)))
                                    (copy-file source destination-full-path)
                                    (chmod destination-full-path #o644))))
                               `(("emacs"
                                  ,#$(local-file (string-append %project-directory "/dot_emacs")))
                                 ("emacs.d/.mc-lists.el"
                                  ,#$(local-file (string-append %project-directory "/private_dot_emacs.d/dot_mc-lists.el")))))))))))

(define home-emacs-service
  (simple-service 'emacs-config
                  home-files-service-type
                  (append (list `(".gnus.el" ,(local-file (string-append %project-directory "/dot_gnus.el"))))
                          (map (lambda (file-name)
                                 `(,(string-append ".emacs.d/" file-name) ,(local-file (string-append %project-directory "/private_dot_emacs.d/" file-name))))
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
                                 "modules/kubernetes.el"
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
                                 "modules/yaml.el"
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
                                 `(,(string-append ".emacs.d/" file-name) ,(local-file (string-append %project-directory "/private_dot_emacs.d/" file-name))))
                               '("insert/guix/gnu/services/service"
                                 "insert/guix/gnu/packages/package"
                                 "insert/guix/gnu/tests/test"
                                 "insert/guix/gnu/system/examples/vm-inherit-image"
                                 "insert/groovy/Jenkinsfile"
                                 "insert/guile/script"
                                 "insert/dotfiles/modules/services/service"
                                 "insert/nix/shell.nix"
                                 "insert/nix/flake.nix"
                                 "insert/kubernetes/kustomization.yaml"
                                 "insert/kubernetes/namespace.yaml"
                                 "insert/kubernetes/release.yaml"
                                 "insert/kubernetes/kustomizeconfig.yaml")))))
