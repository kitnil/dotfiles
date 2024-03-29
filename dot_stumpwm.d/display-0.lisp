(in-package :stumpwm)

(setf *maxsize-border-width* 0)
(setf *message-window-y-padding* 0)
(setf *normal-border-width* 0)
(setf *transient-border-width* 0)
(setq *suppress-frame-indicator* nil)
(setf *float-window-border* 0)
(setf *float-window-title-height* 0)
(bind-super)

(rename-group *default-group-name* "1")
(setq *default-group-name* "1")
;; TODO: Fix VNC
;; (restore-from-file (concat (getenv "HOME") "/.stumpwm.d/desktop-0.lisp"))
(mapcar #'(lambda (x)
            (add-group (current-screen) (write-to-string x) :background t))
        (range 10 :min 2 :step 1))
(add-group (current-screen) "0" :background t :type 'float-group)
(run-commands "gselect 1")

(define-frame-preference "1" (0 NIL T :CLASS "firefox-default" :TITLE "YouTube"))
(define-frame-preference "1" (1 NIL T :CLASS "firefox-default" :TITLE "Picture-in-Picture"))

(when (string-equal (screen-display-string (current-screen)) "DISPLAY=:0.0")
  (mode-line)
  (setf *mouse-focus-policy* :sloppy))

;; XXX: pam-gnupg race condition
;;
;; (let ((uptime-seconds (parse-integer (car (split-string (car (split-string (uiop:read-file-string "/proc/uptime") " ")) ".")))))
;;   (when (< uptime-seconds 60)
;;     (term-shell-command "sh -c 'sleep 5; if gpg-unlock; then exit 0; else read; fi'"
;;                         :terminal 'st
;;                         :color "dark")))

(defun run-frame (group-number &key
                                 (restart? nil)
                                 (frame-0-command nil)
                                 (frame-1-command nil)
                                 (frame-2-command nil))
  (if (string= (group-name (current-group))
               (write-to-string group-number))
      (let ((screen (current-screen))
            (group (current-group)))
        (cond
          ;; Two monitors.
          ((and (> (screen-width screen) 1920)
                (= (length (group-frames group)) 2))
           (if (and (current-window) (not restart?))
               (run-commands "fnext")
               (case (frame-number (tile-group-current-frame group))
                 ((0) (funcall frame-0-command))
                 ((1) (funcall frame-1-command)))))
          ;; Two monitor with frames: big, small, medium
          ((and (> (screen-width screen) 1920)
                (>= (length (group-frames group)) 2))
           (if (and (current-window) (not restart?))
               (run-commands "fnext")
               (case (frame-number (tile-group-current-frame group))
                 ((0) (funcall frame-0-command))
                 ((1) (funcall frame-2-command))
                 ((2) (funcall frame-1-command)))))
          ;; Single monitor.
          (t
           (case (length (group-windows group))
             ((0) (funcall frame-0-command))
             ((1) (funcall frame-1-command))))))
      (run-commands (format nil "gselect ~a" group-number))))

(defcommand group-1-start-programs () ()
  (run-frame 1 :frame-0-command (lambda ()
                                  (run-shell-command "firefox"))
               :frame-1-command (lambda ()
                                  (run-shell-command "run-emacs"))
               :frame-2-command (lambda ()
                                  (emacs-todo-new-window))))

(defcommand group-1-restart-programs () ()
  (run-frame 1 :restart? t
               :frame-0-command (lambda ()
                                  (run-shell-command "pkill emacs; run-emacs"))
               :frame-1-command (lambda ()
                                  (run-shell-command "pkill firefox; firefox"))
               :frame-2-command (lambda ()
                                  (emacs-todo-new-window))))

(defcommand group-2-start-programs () ()
  (run-frame 2
             :frame-0-command
             (lambda ()
               (run-shell-command "vnc client guix"))))

(defcommand group-3-start-programs () ()
  (run-frame 3 :frame-0-command
             (lambda ()
               (run-shell-command "vnc client windows"))))

(defvar *windows-novnc*
  "http://novnc.windows.home.wugi.info/vnc.html")

(defcommand group-4-start-programs () ()
  (run-frame 4
             :frame-0-command
             (lambda ()
               (run-shell-command (firefox-temp-profile nil *windows-novnc*)))))

(defcommand group-5-start-programs () ()
  (run-frame 5 :frame-0-command
             (lambda ()
               (let (;; (window (current-window))
                     )
                 (run-shell-command
                  (join
                   (list
                    "xfreerdp"
                    (concat "/w:" "1920" ;; (window-width window)
                            )
                    (concat "/h:" "1052" ;; (window-height window)
                            )
                    (concat "/u:" "oleg")
                    (concat "/p:" (string-trim '(#\Newline)
                                               (password-store-show "windows.local/oleg")))
                    "/v:windows.local")))))))

(defcommand group-6-start-programs () ()
  (run-frame 6 :frame-0-command (lambda ()
                                  (run-commands "trans-en-ru"))
               :frame-1-command (lambda ()
                                  (run-commands "trans-ru-en"))))

(defcommand group-7-start-programs () ()
  (run-frame 7 :frame-0-command (lambda ()
                                  (run-commands "looking-glass-client"))))

(defcommand group-8-start-programs () ()
  (run-frame 8 :frame-0-command (lambda ()
                                  (if (free-time?)
                                      (run-commands "pulsemixer")
                                      (run-commands "emacs-anywhere")))
               :frame-1-command (lambda ()
                                  (if (free-time?)
                                      (run-shell-command "chromium --app=https://habitica.com/")
                                      (run-shell-command "firefox --new-window https://cerberus.intr/")))))

(defcommand group-9-start-programs () ()
  (run-frame 9 :frame-0-command (lambda ()
                                  (run-commands "mjru-office-shedule-eng"))
               :frame-1-command (lambda ()
                                  (run-commands "mjru-office-shedule-sup"))))

;; (define-frame-preference "1" (1 NIL NIL :CLASS "mpv"))

;; (add-hook *new-window-hook* (lambda (window) ||#
;;                               (when (string= (window-class window) "mpv") ||#
;;                                 (update-fullscreen window 2) ||#
;;                                 (run-commands "move-focus left")))) ||#

;; (add-hook *new-window-hook*
;;           (lambda (window)
;;             (when (string= (window-role window) "PictureInPicture")
;;               (let* ((screen (current-screen))
;;                      (head (current-head))
;;                      (ml (head-mode-line head)))
;;                 (setf (mode-line-mode ml) :hidden)
;;                 (xlib:unmap-window (mode-line-window ml))
;;                 (dolist (group (screen-groups screen))
;;                   (group-sync-head group head))))))
