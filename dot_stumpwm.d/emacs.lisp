(in-package :stumpwm)

(defun switch-to-emacs ()
  (unless (uiop/utility:string-prefix-p "Emacs"
                                        (window-class (current-window)))
    (run-shell-command "stumpish emacsclient")))

(defmacro emacs-bind (key command)
  `(progn
     (defcommand ,(intern (concat "emacs-" command)) () ()
       (emacsclient-eval (format nil "(~a)" ,command)))
     (define-key *top-map* (kbd ,key) ,(concat "emacs-" command))))

(defun emacsclient-command (&rest args)
  (run-shell-command (format nil "emacsclient ~a" (join args))))

(defun emacsclient-eval (command)
  (emacsclient-command (format nil "-e ~s" command)))

(defun emacs-buffer (buffer)
  (emacsclient-command "-s" "chat"
                       (format nil "-e ~s"
                               (format nil "(switch-to-buffer ~s)" buffer))
                       "-c"))

(defun emacs-erc ()
  (mapcar #'(lambda (buffer)
              (emacs-buffer buffer))
          '("#bash" "#bootstrappable" "##C" "#chicken" "#emacs" "#erc" "#fsf"
            "#gdb" "#gnus" "#guile" "#guix" "##linux" "#lisp" "#nixos"
            "#scheme" "#stumpwm")))

(defcommand emacsclient () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "emacsclient -c" '(:class "Emacs")))

(defcommand emacsclient-new () ()
  (run-shell-command "emacsclient -c"))

(defcommand xclip-emacs () ()
  "Open file from clipboard."
  (run-shell-command
   (join (list "exec emacsclient -c" (get-x-selection)))))

(defcommand emacs-vterm () ()
  ""
  (switch-to-emacs)
  (run-shell-command "emacsclient -e '(progn (vterm-toggle))'"))

(defcommand emacs-shell () ()
  ""
  (switch-to-emacs)
  (run-shell-command "emacsclient -e '(progn (shell) (delete-other-windows))'"))

(defcommand helm-tramp () ()
  (progn (run-shell-command "emacsclient --eval '(helm-tramp)'")
         (switch-to-emacs)))

(defcommand emacs-anywhere () ()
  "Run `emacs-anywhere'."
  (run-shell-command "emacs-anywhere"))


;;;
;;; Project
;;;

(defcommand wi-project-ivy () ()
  (progn (run-shell-command "emacsclient --eval '(wi-project-ivy)'")
         (switch-to-emacs)))

(defcommand wi-project-browse-at-remote () ()
  (progn (run-shell-command "emacsclient --eval '(wi-project-browse-at-remote)'")
         (switch-to-emacs)))


;;;
;;; EMMS
;;;

(defcommand emacs-mms-next () ()
  (emacsclient-eval (format nil "(~a)" "emms-next")))

(defcommand emacs-emms-previous () ()
  (emacsclient-eval (format nil "(~a)" "emms-previous")))

(defcommand emacs-emms-pause () ()
  (emacsclient-eval (format nil "(~a)" "emms-pause")))

(defcommand emacs-emms-random () ()
  (emacsclient-eval (format nil "(~a)" "emms-random")))

(defcommand emms () ()
  (switch-to-emacs)
  (run-shell-command
   (format nil "emacsclient -e ~s"
           (sb-unicode:lowercase
            (write-to-string
             '(progn (emms) (delete-other-windows)))))))


;;;
;;; org-mode
;;;

(defcommand emacs-org-capture () ()
  "Capture URL with Emacs Org from GUI clipboard"
  (run-shell-command
   (join (list "exec"
               (concat (getenv "HOME") "/bin/emacs-org-capture")
               (get-x-selection)))))

(defcommand org () ()
  (progn (run-shell-command "emacsclient --eval '(plain-org-wiki)'")
         (switch-to-emacs)))

(defcommand org-agenda () ()
  (progn (run-shell-command "emacsclient --eval '(org-agenda)'")
         (switch-to-emacs)))

(defcommand emacs-todo () ()
  (progn (run-shell-command (format nil "emacsclient --eval '(find-file ~s)'" "~/src/org/todo.org"))
         (switch-to-emacs)))


;;;
;;; Mail
;;;

(defcommand gnus () ()
  (run-shell-command "emacsclient --eval '(gnus)'")
  (switch-to-emacs))

(defcommand gnus-new-window () ()
  (run-shell-command (format nil "emacsclient -c -e ~s"
                             (sb-unicode:lowercase (write-to-string '(gnus))))))


;;;
;;; Guix
;;;

(defcommand guix () ()
  (switch-to-emacs)
  (run-shell-command "magit ~/src/guix"))

(defcommand guix-wigust () ()
  (progn (run-shell-command "emacsclient --eval '(let ((default-directory (expand-file-name \"~/src/guix-wigust/guix/wigust/packages/\"))) (counsel-find-file))'")
         (switch-to-emacs)))

(defcommand emacs-guix-edit () ()
  (let ((clipboard (get-x-selection)))
    (message (format nil "Guix edit ~a" clipboard))
    (switch-to-emacs)
    (run-shell-command (format nil "emacsclient -e '(guix-edit ~s)'" clipboard))))


;;;
;;; RSS
;;;

(defcommand elfeed () ()
  (switch-to-emacs)
  (run-shell-command
   (format nil "emacsclient -e ~s"
           (sb-unicode:lowercase
            (write-to-string
             '(progn (elfeed) (delete-other-windows)))))))

(defcommand elfeed-new-window () ()
  (run-shell-command
   (format nil "emacsclient -c -e ~s" (sb-unicode:lowercase (write-to-string '(elfeed))))))
