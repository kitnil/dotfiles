(in-package :stumpwm)

(setf *maxsize-border-width* 0)
(setf *message-window-y-padding* 0)
(setf *normal-border-width* 0)
(setf *transient-border-width* 0)
(bind-super)

(rename-group *default-group-name* "1")
(setq *default-group-name* "1")
(restore-from-file (concat (getenv "HOME") "/src/dotfiles/oleg/.stumpwm.d/desktop/0.lisp"))
;; (mapcar #'(lambda (x)
;;             (add-group (current-screen) (write-to-string x) :background t))
;;         (range 10 :min 2 :step 1))
(add-group (current-screen) "0" :background t :type 'float-group)
(gselect "1")

(defun run-frame (group-number &key (frame-0-command nil) (frame-1-command nil))
  (if (= (parse-integer (group-name (current-group))) group-number)
      (if (current-window)
          (run-commands "fnext")
          (let ((screen (current-screen))
                (group (current-group)))
            (if (and (> (screen-width screen) 1920)
                     (= (length (group-frames group)) 2))
                (if (= (frame-number (tile-group-current-frame group)) 0)
                    (funcall frame-0-command)
                    (funcall frame-1-command)))))
      (run-commands (format nil "gselect ~a" group-number))))

(defcommand group-1-start-programs () ()
  (run-frame 1 :frame-0-command (lambda () (run-shell-command "run-emacs"))
               :frame-1-command (lambda () (run-shell-command "quassel"))))

(defcommand group-2-start-programs () ()
  (run-frame 2 :frame-0-command (lambda () (gnus-new-window)) 
               :frame-1-command (lambda () (elfeed-new-window))))

(defcommand group-3-start-programs () ()
  (run-frame 3 :frame-0-command (lambda ()
                                  (run-shell-command
                                   (if (free-time?)
                                       "chromium --new-window https://home-s2x8742.slack.com/"
                                       "chromium --new-window https://mjru.slack.com/")))
               :frame-1-command (lambda ()
                                  (run-shell-command
                                   (if (free-time?)
                                       "chromium --new-window https://jenkins.wugi.info/view/Failed/"
                                       "chromium --new-window https://jenkins.intr/view/Failed/")))))

(defcommand group-4-start-programs () ()
  (run-frame 4 :frame-0-command (lambda ()
                                  (run-shell-command
                                   "chromium --new-window https://grafana.intr/d/000000042/netflow?orgId=1"))
               :frame-1-command (lambda ()
                                  (run-shell-command
                                   "chromium --new-window https://grafana.intr/d/6QgXJjmik/upstream-interfaces-traffic?orgId=1"))))

(defcommand group-5-start-programs () ()
  (run-frame 5 :frame-0-command (lambda ()
                                  (run-shell-command
                                   (if (free-time?) "kodi" (xpanes-dh-ssh))))
               :frame-1-command (lambda ()
                                  (run-commands "xpanes-ssh-nginx"))))

(defcommand group-6-start-programs () ()
  (run-frame 6 :frame-0-command (lambda ()
                                  (run-commands "trans-en-ru"))
               :frame-1-command (lambda ()
                                  (run-commands "trans-ru-en"))))

(defcommand group-7-start-programs () ()
  (run-frame 7 :frame-0-command (lambda ()
                                  (run-commands "repl-guix"))
               :frame-1-command (lambda ()
                                  (run-commands "repl-nix-unstable"))))

(defcommand group-8-start-programs () ()
  (run-frame 8 :frame-0-command (lambda ()
                                  (run-shell-command
                                   (if (free-time?)
                                       (elfeed)
                                       "firefox --new-window https://cerberus.intr/")))
               :frame-1-command (lambda ()
                                  (run-commands "emacs-anywhere"))))

(defcommand group-9-start-programs () ()
  (run-frame 9 :frame-0-command (lambda ()
                                  (run-commands "majordomo-office-shedule-eng"))
               :frame-1-command (lambda ()
                                  (run-commands "majordomo-office-shedule-sup"))))
