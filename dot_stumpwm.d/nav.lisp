(in-package :stumpwm)

(require :globalwindows)

(setf *new-frame-action* :empty)

(setf *window-format* "%m%n%s %c %50t")

(setf *maximum-completions* 25)


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

(defcommand xmenu () ()
  (run-shell-command "xmenu.sh"))


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
       (emacsclient-eval "(helm-buffers-list)"))
      (t (globalwindows:global-windowlist)))))

(defcommand next-in-frame-custom () ()
  (let ((window (current-window)))
    (cond
      ((string= (window-class window) "Emacs")
       (emacsclient-eval "(next-buffer)"))
      ((string= (window-class window) "mpv")
       (window-send-string ">"))
      ((some (lambda (str)
               (string= str (window-class window)))
             '("Nightly" "Chromium-browser" "Xfce4-terminal"))
       (send-fake-key window (kbd "C-Page_Down")))
      ((and (string= (window-class window) "XTerm")
            (string= (window-name window) "tmux"))
       (send-fake-key window (kbd "C-b"))
       (send-fake-key window (kbd "n")))
      (t (next-in-frame)))))

(defcommand prev-in-frame-custom () ()
  (let ((window (current-window)))
    (cond
      ((string= (window-class window) "Emacs")
       (emacsclient-eval "(previous-buffer)"))
      ((string= (window-class window) "mpv")
       (window-send-string "<"))
      ((some (lambda (str)
               (string= str (window-class window)))
             '("Nightly" "Chromium-browser" "Xfce4-terminal"))
       (send-fake-key window (kbd "C-Page_Up")))
      ((and (string= (window-class window) "XTerm")
            (string= (window-name window) "tmux"))
       (send-fake-key window (kbd "C-b"))
       (send-fake-key window (kbd "p")))
      (t (prev-in-frame)))))

(defcommand keybinding-s-k () ()
  (let ((window (current-window)))
    (cond
      ((uiop/utility:string-prefix-p "repl-nix" (window-title window))
       (kill-window window))
      ((string= (window-class window) "Emacs")
       (emacsclient-eval "(kill-buffer (window-buffer (frame-selected-window)))"))
      ((string= (window-class window) "Nightly")
       (send-fake-key window (kbd "M-w")))
      ((and (string= (window-class window) "XTerm")
            (string= (window-name window) "tmux"))
       (sb-thread:make-thread
        (lambda ()
          (send-fake-key window (kbd "C-b"))
          (send-fake-key window (kbd "x"))
          (sleep 0.01)
          (send-fake-key window (kbd "y")))))
      (t (delete-window window)))))

(defcommand keybinding-s-o () ()
  (let ((window (current-window)))
    (cond
      ((string= (window-class window) "Emacs")
       (send-fake-key window (kbd "M-x"))
       (window-send-string "ffap"))
      (t (emacs-anywhere)))))

(defcommand keybinding-s-r () ()
  (let ((window (current-window)))
    (cond
      ((string= (window-class window) "Xfce4-terminal")
       (send-fake-key window (kbd "C-F")))
      ((and (string= (window-class window) "XTerm")
            (string= (window-name window) "tmux"))
       (send-fake-key window (kbd "C-b"))
       (send-fake-key window (kbd "[")))
      (t (repl-guile)))))

(defcommand keybinding-s-= () ()
  (let ((window (current-window)))
    (cond
      ((string= (window-class window) "Emacs")
       (send-fake-key window (kbd "C-x"))
       (send-fake-key window (kbd "C-=")))
      ((string= (window-class window) "Nightly")
       (send-fake-key window (kbd "M-=")))
      ((some (lambda (str)
               (string= str (window-class window)))
             '("Chromium-browser" "Xfce4-terminal"))
       (send-fake-key window (kbd "C-+")))
      ((string= (window-class window) "XTerm")
       (send-fake-key window (kbd "S-KP_Add")))
      ((string= (window-class window) "Alacritty")
       (send-fake-key window (kbd "C-="))))))

(defcommand keybinding-s-- () ()
  (let ((window (current-window)))
    (cond
      ((string= (window-class window) "Emacs")
       (send-fake-key window (kbd "C-x"))
       (send-fake-key window (kbd "C--")))
      ((string= (window-class window) "Nightly")
       (send-fake-key window (kbd "M--")))
      ((some (lambda (str)
               (string= str (window-class window)))
             '("Chromium-browser" "Xfce4-terminal"))
       (send-fake-key window (kbd "C--")))
      ((string= (window-class window) "XTerm")
       (send-fake-key window (kbd "S-KP_Subtract")))
      ((string= (window-class window) "Alacritty")
       (send-fake-key window (kbd "C--"))))))

(defcommand keybinding-s-x () ()
  (let ((clipboard (get-x-selection)))
    (cond ((string-contains "AC_" clipboard)
           (sb-thread:make-thread
            (lambda ()
              (run-shell-command (format nil "notify-send ~s"
                                         (string-trim '(#\Newline)
                                                      (run-shell-command (format nil "hms web unix ~a" clipboard)
                                                                         t)))))))
          ((= (length clipboard) 24)
           (mjru-mongo-development-id-object)))))


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


;;;
;;; Clipboard
;;;

(defcommand window-send-clipboard () ()
  (window-send-string (get-x-selection)))


;;;
;;; Help
;;;

(defcommand display-0-keys () ()
  (term-shell-command
   (join (list "less"
               (concat (getenv "HOME") "/.local/share/chezmoi/dot_stumpwm.d/display-0.lisp")))))

(defcommand delete-window! (&optional (window (current-window))) ()
  (delete-window window)
  (when (not (current-window))
    (run-commands "fnext")))
