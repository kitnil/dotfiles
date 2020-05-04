(in-package :stumpwm)

(setf *maxsize-border-width* 0)
(setf *message-window-y-padding* 0)
(setf *normal-border-width* 0)
(setf *transient-border-width* 0)
(bind-super)

(rename-group *default-group-name* "1")
(restore-from-file (concat (getenv "HOME") "/src/dotfiles/oleg/.stumpwm.d/desktop/0.lisp"))
;; (mapcar #'(lambda (x)
;;             (add-group (current-screen) (write-to-string x) :background t))
;;         (range 10 :min 2 :step 1))
(add-group (current-screen) "0" :background t :type 'float-group)

(defun frame-parameters-display-0 ()
  (mapcar (lambda (group)
            (define-frame-preference group (0 NIL T :CLASS "XTerm" :TITLE "mbsync-majordomo"))
            (define-frame-preference group (0 NIL T :CLASS "XTerm" :TITLE "youtube-dl"))
            (define-frame-preference group (4 NIL T :CLASS "XTerm" :TITLE "youtube-dl-music"))
            (define-frame-preference group (0 NIL T :TITLE "xpanes-top"))
            (define-frame-preference group (0 NIL T :INSTANCE "music.youtube.com"))
            (define-frame-preference group (0 NIL T :INSTANCE "www.youtube.com__playlist"))
            (define-frame-preference group (1 NIL NIL :CLASS "obs"))
            (define-frame-preference group (1 NIL NIL :CLASS "quassel"))
            (define-frame-preference group (3 NIL NIL :CLASS "Dragon"))
            (define-frame-preference group (3 NIL NIL :TITLE "alerta-top"))
            (define-frame-preference group (4 NIL NIL :TITLE "pulsemixer"))
            (define-frame-preference group (4 NIL T :TITLE "notmuch"))
            (define-frame-preference group (4 NIL T :CLASS "mpv" :TITLE "emacs-emms"))
            (define-frame-preference group (5 NIL T :CLASS "Vncviewer" :TITLE "guixsd"))
            (define-frame-preference group (5 NIL T :CLASS "mpv" :TITLE "firefox")))
          '("Default" "1")))

(frame-parameters-display-0)

(defun run-frame (group-number &key (frame-0-command nil) (frame-1-command nil))
  (if (= (parse-integer (group-name (current-group))) group-number)
      (if (current-window)
          (run-commands "fnext")
          (let ((screen (current-screen))
                (group (current-group)))
            (if (and (> (screen-width screen) 1920)
                     (= (length (group-frames group)) 2))
                (if (= (frame-number (tile-group-current-frame group)) 0)
                    (run-commands frame-0-command)
                    (run-commands frame-1-command)))))
      (run-commands (format nil "gselect ~a" group-number))))

(defcommand group-2-start-programs () ()
  (run-frame 2 :frame-0-command "gnus-new-window" :frame-1-command "elfeed-new-window"))

(defcommand group-3-frame-0-command () ()
  (run-shell-command (if (free-time?)
                         "chromium --new-window https://home-s2x8742.slack.com/"
                         "chromium --new-window https://mjru.slack.com/")))

(defcommand group-3-frame-1-command () ()
  (run-shell-command (if (free-time?)
                           "chromium --new-window https://jenkins.wugi.info/view/Failed/"
                           "chromium --new-window https://jenkins.intr/view/Failed/")))

(defcommand group-3-start-programs () ()
  (run-frame 3 :frame-0-command "group-3-frame-0-command" :frame-1-command "group-3-frame-1-command"))

(defcommand group-4-frame-0-command () ()
  (run-shell-command "chromium --new-window https://grafana.intr/d/000000042/netflow?orgId=1"))

(defcommand group-4-frame-1-command () ()
  (run-shell-command "chromium --new-window https://grafana.intr/d/6QgXJjmik/upstream-interfaces-traffic?orgId=1"))

(defcommand group-4-start-programs () ()
  (run-frame 4 :frame-0-command "group-4-frame-0-command" :frame-1-command "group-4-frame-1-command"))

(defcommand group-5-frame-0-command () ()
  (run-shell-command (if (free-time?) "kodi" (xpanes-dh-ssh))))

(defcommand group-5-start-programs () ()
  (run-frame 5 :frame-0-command "group-5-frame-0-command" :frame-1-command "xpanes-ssh-nginx"))

(defcommand group-6-start-programs () ()
  (run-frame 6 :frame-0-command "trans-en-ru" :frame-1-command "trans-ru-en"))

(defcommand group-7-start-programs () ()
  (run-frame 7 :frame-0-command "repl-guix" :frame-1-command "repl-nix-unstable"))

(defcommand group-8-frame-0-command () ()
  (run-shell-command
   (if (free-time?)
       (elfeed)
       "firefox --new-window https://cerberus.intr/")))

(defcommand group-8-start-programs () ()
  (run-frame 8 :frame-0-command "group-8-frame-0-command" :frame-1-command "emacs-anywhere"))

(defcommand majordomo-office-shedule-eng () ()
  (run-shell-command "firefox --new-window https://office.majordomo.ru/shedule2/10"))

(defcommand majordomo-office-shedule-sup () ()
  (run-shell-command "firefox --new-window https://office.majordomo.ru/shedule2/2"))

(defcommand group-9-start-programs () ()
  (run-frame 9 :frame-0-command "majordomo-office-shedule-eng" :frame-1-command "majordomo-office-shedule-sup"))
