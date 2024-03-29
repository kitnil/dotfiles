(defun deadgrep-kvm (str)
  (interactive "sString to search: ")
  (let ((default-directory (expand-file-name "~/src/gitlab.intr/vds/vds-xenial")))
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
               (expand-file-name (format "~/src/gitlab.intr/webservices/apache2-php%s" (number-to-string version))))
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

;; TODO: Fix vterm-toggle.el tramp-auto-auth-secrets.el helm-tramp.el
;; (with-eval-after-load 'emacs-startup-hook
;;   (load (expand-file-name "~/archive/src/vterm-toggle/vterm-toggle.el") t)
;;   (load (expand-file-name "~/.emacs.d/tramp-auto-auth-secrets.el") t)
;;   (load (expand-file-name "~/src/emacs-helm-tramp/helm-tramp.el") t))


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


;;;
;;; wi-utils
;;;

;; Addional procedures which depends on wi-utils

(defun wi-servers-list-xpanes-open-tail-taskexecutor ()
  "Tail taskexecutor logs."
  (interactive)
  (wi-servers-list-xpanes-terminal (mapcar #'car (bui-list-get-marked-args 'general))
                                   "ssh -t {} -- sudo tail -f /var/log/taskexecutor.log"))

;; TODO: Bind wi-servers-list-xpanes-open-tail-taskexecutor
;; (define-key map (kbd "T") 'wi-servers-list-xpanes-open-tail-taskexecutor)

(defun wi-servers-list-open-terminal ()
  "Open `terminal-here' with selected servers."
  (interactive)
  (mapcar (lambda (entry)
            (let ((host (car entry)))
              (terminal-here (concat "/ssh:" host ":"))))
          (bui-list-get-marked-args 'general)))

;; TODO: Bind wi-servers-list-open-terminal
;; (define-key map (kbd "s") 'wi-servers-list-open-terminal)



(when (file-exists-p "~/.emacs.d/modules/mjru-network.el")
  (load (expand-file-name "~/.emacs.d/modules/mjru-network.el")))

(defun mjru-connect-br1-mr14.intr ()
  (interactive)
  (vterm)
  (vterm-send-string "connect br1-mr14.intr")
  (vterm-send-return))

(defun mjru-find-file-kubernetes ()
  (mapc (lambda (host)
          (find-file (concat "/ssh:root@" host ":")))
        '("78.108.87.50" "178.250.246.69" "178.250.245.80" "178.250.247.88")))
