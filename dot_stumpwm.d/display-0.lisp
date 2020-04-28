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
          '("Default" "1"))
  (define-frame-preference "2" (0 NIL NIL :CLASS "Emacs"))
  (define-frame-preference "2" (0 NIL T :CLASS "Firefox" :TITLE "jenkins"))
  (define-frame-preference "5" (0 NIL NIL :CLASS "Emacs")))

(frame-parameters-display-0)

(defcommand group-2-start-programs () ()
  (run-commands "gselect 2")
  (unless (current-window)
    (run-shell-command (format nil "emacsclient -c -e ~s"
                               (sb-unicode:lowercase (write-to-string '(gnus)))))
    (if (y-or-n-p "Fetch mail? ") (notmuch))))

(defcommand group-3-start-programs () ()
  (run-commands "gselect 3")
  (unless (current-window)
    (run-shell-command (if (free-time?)
                           "chromium --new-window https://home-s2x8742.slack.com/"
                           "chromium --new-window https://mjru.slack.com/"))))

(defcommand group-4-start-programs () ()
  (run-commands "gselect 4")
  (unless (current-window)
    (run-shell-command (if (free-time?)
                           "chromium --new-window https://jenkins.wugi.info/view/Failed/"
                           "chromium --new-window https://jenkins.intr/view/Failed/"))))

(defcommand group-5-start-programs () ()
  (run-commands "gselect 5")
  (unless (current-window)
    (run-shell-command (if (free-time?)
                           "kodi"
                           (xpanes-dh-ssh)))))

(defcommand group-6-start-programs () ()
  (run-commands "gselect 6")
  (unless (current-window)
    (trans-en-ru)))

(defcommand group-7-start-programs () ()
  (run-commands "gselect 7")
  (unless (current-window)
    (repl-nix-unstable)))

(defcommand group-8-start-programs () ()
  (run-commands "gselect 8")
  (unless (current-window)
    (run-shell-command (if (free-time?)
                           (format nil "emacsclient -c -e ~s"
                                   (sb-unicode:lowercase (write-to-string '(elfeed))))
                           "firefox --new-window https://cerberus.intr/"))))

(defcommand group-9-start-programs () ()
  (run-commands "gselect 9")
  (unless (current-window)
    (emacs-anywhere)))
