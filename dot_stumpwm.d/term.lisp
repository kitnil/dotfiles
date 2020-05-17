(in-package :stumpwm)

(defvar *term-execute-flag* "-e")


;;;
;;; Xterm
;;;

(sb-posix:setenv "TERM_PS1" "gentoo" 1)

(defvar *xterm-command* "/home/oleg/.guix-profile/bin/xterm")
(defvar *xterm-big-command*
  (join '("exec" "/home/oleg/.guix-profile/bin/xterm"
          "-fa" "Monospace" "-fs" "24")))
(defvar *xterm-no-scrollbar* "+sb")
(defvar *xterm-theme-dark* "-bg black -fg white")
(defvar *xterm-theme-light* "-bg white -fg black")

(defun xterm-command (&key
                        (color (if dark-theme "dark" "light"))
                        (command nil)
                        (title nil)
                        (font nil)
                        (scrollbar nil))
  (join `(,*xterm-command*
          ;; Make sure XTerm terminal size is appropriate for current StumpWM frame.
          ,@(if font
                font
                (if (small-framep)
                    '("-fa" "Monospace" "-fs" "8")
                    '()))
          "-sl" "1000000" ;number of lines
          ,(if scrollbar "-sb" *xterm-no-scrollbar*)
          ,(if (string= color "light") *xterm-theme-light* *xterm-theme-dark*)
          ,@(if title `("-title" ,title) '())
          ,@(if command `("-e" ,command) '()))))

(defcommand run-xterm-command (cmd &optional collect-output-p)
    ((:shell "/bin/sh -c "))
  "Run the specified shell command in XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list "/home/oleg/.guix-profile/bin/xterm -name" cmd "-e" cmd)))
            :wait nil))

(defcommand run-or-raise-xterm () ()
  "Start or focus XTerm."
  (run-or-raise
   (join (list *xterm-command* *xterm-theme-dark* *xterm-no-scrollbar*))
   '(:class "XTerm")))

(defcommand run-xterm-named (title) ((:string "title: "))
  "Start or focus XTerm."
  (run-shell-command (xterm-command :color (if dark-theme "dark" "light")
                                    :title title
                                    :scrollbar t)))

(defcommand run-xterm () ()
  "Start or focus XTerm."
  (run-prog *shell-program*
            :args (list "-c" (xterm-command :scrollbar t))
            :wait nil))

(defcommand xterm-dark-no-scrollbar () ()
  "Start or focus XTerm."
  (run-shell-command (xterm-command :color (if dark-theme "light" "dark"))))

(defcommand xterm-name (cmd &optional collect-output-p)
    ((:string "window name: "))
  "Run the specified shell command in XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list "xterm -name" cmd)))
            :wait nil))

(defcommand xterm-big () ()
  "Start XTerm with big fonts."
  (run-shell-command *xterm-big-command*))

(defcommand xterm-big-screen () ()
  "Start XTerm with big fonts."
  (run-shell-command
   (concat *xterm-big-command* " -e screen")))


;;;
;;; St
;;;

(defvar *st-command* "exec st")
(defvar *st-exec-flag* "-e")
(defvar *st-font* "Monospace:size=12")
(defvar *st-font-flag* "-f")

(defcommand st () ()
  "Start st."
  (run-shell-command "st -f 'Monospace:size=12'"))

(defcommand st-tmux () ()
  "Start st with tmux."
  (run-shell-command "st -f 'Monospace:size=12' -e tmux"))


;;;
;;; XFCE
;;;

(defcommand xfce-terminal () ()
  (run-shell-command "xfce4-terminal"))

(defcommand run-or-raise-xfce-terminal () ()
  (run-or-raise "xfce4-terminal"
                '(:class "Xfce4-terminal")))


;;;
;;; Screen
;;;

(defcommand screen
    (session &optional collect-output-p)
    ((:string "session name: "))
  "Run `screen' session."
  (run-prog *shell-program*
            :args
            (list "-c"
                  (join (list "env" "STY=" ; Do not complain `$STY' in `screen'.
                              "xterm" "-title" session
                              "-e" "screen" "-S" session)))
            :wait nil))


;;;
;;; Wrappers
;;;

(defun term-shell-command (command &key
                                     (terminal 'xterm)
                                     (color (if dark-theme "dark" "light"))
                                     (font nil)
                                     (title nil)
                                     (scrollbar nil))
  (run-shell-command
   (let ((terminal-name (string-downcase (symbol-name terminal))))
     (case terminal
       ((xterm)
        (xterm-command :color color :command command :font font :title title :scrollbar scrollbar))
       ((st)
        (join `(,terminal-name
                ,*st-font-flag* ,(if font font *st-font*)
                ,@(if title (list "-t" title) '())
                ,*st-exec-flag* ,command)))))))

