(defun deadgrep-kvm (str)
  (interactive "sString to search: ")
  (let ((default-directory (expand-file-name "~/majordomo/vds/vds-xenial")))
    (deadgrep str)))


;;;
;;; Mail
;;;

;; TODO: Majordomo SMTP
;; (with-eval-after-load 'sendmail
;;   (setq send-mail-function #'smtpmail-send-it)
;;   (setq smtpmail-smtp-user "pyhalov@majordomo.ru")
;;   (setq smtpmail-smtp-server "router.majordomo.ru"))


;;;
;;; Snippets
;;;

(defun majordomo-php-insert ()
  (interactive)
  (insert
   (prin1-to-string
    '(mapcar (lambda (version)
               (expand-file-name (format "~/majordomo/webservices/apache2-php%s" (number-to-string version))))
             '(52 53 54 55 56 70 71 72 73 74)))))


;;;
;;; Documentation
;;;

(defun sup-capture ()
  (interactive)
  (find-file (expand-file-name "~/src/work/doc/todo.org"))
  (split-window-right)
  (find-file (expand-file-name "~/src/tech-info/tech.texi"))
  ;; (setq compilation-ask-about-save nil)
  (compile (mapconcat 'identity
                      '("makeinfo" "--no-number-sections"
                        "--css-ref=https://www.gnu.org/software/gnulib/manual.css"
                        "--no-split" "--html" "tech.texi"

                        "&&" "mv" "tech.html" "/var/www/techinfo.intr/index.html")
                      " ")))

(load (expand-file-name "~/archive/src/vterm-toggle/vterm-toggle.el") t)
(load (expand-file-name "~/.emacs.d/tramp-auto-auth-secrets.el") t)
(load (expand-file-name "~/src/emacs-helm-tramp/helm-tramp.el") t)
(load (expand-file-name "~/archive/src/tmp/tmp.iGP0dpFwEX/mj-servers.el") t)
(load (expand-file-name "~/archive/src/tmp/tmp.iGP0dpFwEX/wi-project.el") t)


;;;
;;; SSH
;;;

(defun wi-find-docker-gitlab-file-name ()
  (interactive)
  (let ((file-name (split-string (buffer-file-name) "/")))
    (find-file
     (mapconcat 'identity
                (if (string= (nth 1 file-name) "docker:gitlab:")
                    `("/srv/src/gitlab-ce" ,@(nthcdr 7 file-name))
                    `("/docker:gitlab:/opt/gitlab/embedded/service/gitlab-rails"
                      ,@(nthcdr 4 file-name)))
                "/"))))


