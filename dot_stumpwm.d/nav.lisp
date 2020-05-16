(in-package :stumpwm)

(require :globalwindows)

(setf *new-frame-action* :empty)

(setf *window-format* "%m%n%s %c %50t")


;;;
;;; Mouse
;;;

;; Don't set it to “sloppy”,
;; because it could switch window after switch desktop
(setf *mouse-focus-policy* :click)

(defcommand mouse-click () ()
  (setf *mouse-focus-policy* :click))

(defcommand mouse-sloppy () ()
  (setf *mouse-focus-policy* :sloppy))


;;;
;;; Desktop
;;;

(defcommand desktop-restore (desktop rules) ((:string "Restore desktop: ")
                                             (:string "Restore rules: "))
  (let ((desktop (format nil "~a/.stumpwm.d/desktop/~a.lisp" (getenv "HOME") desktop))
        (rules (format nil "~a/.stumpwm.d/rules/~a.lisp" (getenv "HOME") rules)))
    (message (format nil "Restore desktop from ~s file." desktop))
    (message (format nil "Restore rules from ~s file." rules))
    (restore-from-file desktop)
    (clear-window-placement-rules)
    (restore-window-placement-rules rules)
    (place-existing-windows)))

(defcommand dump-group-to-file (file) (:rest "Dump To File: ")
  "Dumps the frames of the current group of the current screen to the named file."
  (dump-to-file (dump-group (current-group)) file))


;;;
;;; Frames
;;;

(defun grid-split-3x3 ()
  (flet ((split ()
           (progn
             (hsplit)
             (vsplit)
             (fnext)
             (fnext)
             (vsplit)
             (fnext)
             (fnext))))
    (progn
      (hsplit)
      (vsplit)
      (split)
      (split)
      (vsplit)
      (split)
      (split))))

(defcommand scroll-other-window () ()
  (stumpwm:run-commands "fother" "window-send-string  " "fother"))

(defcommand warp-mouse-active-frame () ()
  "Move mouse cursor to the top right of current frame."
  (let* ((current-frame (tile-group-current-frame (current-group)))
         (pointer-x (- (+ (frame-x current-frame)
                          (frame-width current-frame))
                       100))
         (pointer-y (+ 100 (frame-y current-frame))))
    (warp-pointer (current-screen) pointer-x pointer-y)))

(defcommand wi-sync-all-frame-windows () ()
  (sync-all-frame-windows (current-group)))

(defcommand other-in-frame-or-fother () ()
    (let* ((group (current-group))
           (frame (tile-group-current-frame group)))
      (if (> (length (frame-sort-windows group frame)) 1)
          (other-in-frame)
          (fother))))


;;;
;;; Windows
;;;

(defcommand window-resize-by-half-horizontal () ()
  ""
  (resize (- (round (/ (parse-integer (format-expand *window-formatters* "%w" (current-window))) 2.0))) 0))

(defcommand window-resize-by-half-vertical () ()
  ""
  (resize 0 (- (round (/ (parse-integer (format-expand *window-formatters* "%h" (current-window))) 2)))))

(defun current-window-width ()
  (format-expand *window-formatters* "%w" (current-window)))

(defun current-window-height ()
  (format-expand *window-formatters* "%h" (current-window)))

(defcommand global-windowlist-custom () ()
  (let ((window (current-window)))
    (cond
      ((and window (string= (window-class window) "Emacs"))
       (emacsclient-eval "(ivy-switch-buffer)"))
      (t (globalwindows:global-windowlist)))))

(defcommand next-in-frame-custom () ()
  (let ((window (current-window)))
    (cond
      ((string= (window-class window) "Emacs")
       (emacsclient-eval "(next-buffer)"))
      (t (next-in-frame)))))

(defcommand prev-in-frame-custom () ()
  (let ((window (current-window)))
    (cond
      ((string= (window-class window) "Emacs")
       (emacsclient-eval "(previous-buffer)"))
      (t (prev-in-frame)))))

(defcommand delete-or-kill-window () ()
  (let ((window (current-window)))
    (cond
      ((uiop/utility:string-prefix-p "repl-nix" (window-title window))
       (kill-window window))
      ((string= (window-class window) "Emacs")
       (emacsclient-eval "(kill-buffer (window-buffer (frame-selected-window)))"))
      (t (delete-window window)))))


;;;
;;; Small frame
;;;

(defvar *small-frame-width* 954)

(defun small-framep ()
  (let ((group (current-group)))
    (if (string= (class-name (class-of group)) "FLOAT-GROUP")
        nil
        (<= (frame-width (tile-group-current-frame group)) *small-frame-width*))))

(defcommand current-frame-smallp () ()
  (if (small-framep)
      (progn (message "small") 1)
      (progn (message "big") 0)))
