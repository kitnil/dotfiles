(in-package :stumpwm)

(defcommand s-I () ()
  "Open YouTube if free-time? is nil or mjru-servers."
  (if (free-time?)
      (youtube)
      (mjru-servers)))

(defcommand kbd-C-s-a () ()
  "Workaround for SVEN KB-C3800W."
  (xmodmap)
  (mjru-pass-route))

(defun bind-super ()
  (define-key *top-map* (kbd "C-s-e") "editor")
  (define-key *top-map* (kbd "C-s-w") "chromium")
  (define-key *top-map* (kbd "C-s-W") "chromium-new-window")
  (define-key *top-map* (kbd "C-S-s-RET") "rofi-mycli")
  (define-key *top-map* (kbd "C-s-s") "neofetch")
  (define-key *top-map* (kbd "s-+") "pavucontrol")
  (define-key *top-map* (kbd "s-_") "volume-toggle")
  (define-key *top-map* (kbd "s-r") "keybinding-s-r")
  (define-key *top-map* (kbd "M-s-r") "repl-ghci")
  (define-key *top-map* (kbd "s-R") "repl-guix")
  (define-key *top-map* (kbd "C-s-r") "repl-python")
  (define-key *top-map* (kbd "C-M-s-RET") "repl-r")
  (define-key *top-map* (kbd "C-s-R") "repl-nix")
  (define-key *top-map* (kbd "C-M-s-R") "repl-node")
  (define-key *top-map* (kbd "s-f") "fullscreen")
  (define-key *top-map* (kbd "s-D") "mjru-docker-pull")
  (define-key *top-map* (kbd "s-H") "glances")
  (define-key *top-map* (kbd "s-t") "top")
  (define-key *top-map* (kbd "s-T") "tometips")
  (define-key *top-map* (kbd "s-h") "htop")
  (define-key *top-map* (kbd "C-s-C") "rofi-drun")
  (define-key *top-map* (kbd "S-s-RET") "rofi-ssh")
  (define-key *top-map* (kbd "s-\"") "global-windowlist")
  (define-key *top-map* (kbd "s-quoteright") "global-windowlist-custom")
  (define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-decrease")
  (define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-increase")
  (define-key *top-map* (kbd "s--") "keybinding-s--")
  (define-key *top-map* (kbd "s-=") "keybinding-s-=")
  (define-key *top-map* (kbd "s-KP_Add") "volume-increase")
  (define-key *top-map* (kbd "s-KP_Subtract") "volume-decrease")
  (define-key *top-map* (kbd "s-RET") "run-xterm")
  (define-key *top-map* (kbd "s-e") "emacsclient")
  (define-key *top-map* (kbd "s-E") "emacsclient-new")
  (define-key *top-map* (kbd "s-l") "emacs-todo")
  (define-key *top-map* (kbd "s-m") "mpv")
  (define-key *top-map* (kbd "s-M") "music-mpv")
  (define-key *top-map* (kbd "s-n") "next-in-frame-custom")
  (define-key *top-map* (kbd "s-p") "prev-in-frame-custom")
  (define-key *top-map* (kbd "s-w") "firefox")
  (define-key *top-map* (kbd "s-W") "firefox-new-window")
  (define-key *top-map* (kbd "s-q") "delete")
  (define-key *top-map* (kbd "s-Q") "remove")
  (define-key *top-map* (kbd "s-quoteleft") "emacs-vterm")
  (define-key *top-map* (kbd "s-~") "display-0-keys")
  (define-key *top-map* (kbd "s-o") "keybinding-s-o")
  (define-key *top-map* (kbd "s-j") "run-or-raise-xterm")
  (define-key *top-map* (kbd "M-s-j") "run-xterm-light")
  (define-key *top-map* (kbd "s-J") "jenkins")
  (define-key *top-map* (kbd "s-g") "gnus")
  (define-key *top-map* (kbd "s-G") "notmuch")
  (define-key *top-map* (kbd "s-C") "org")
  (define-key *top-map* (kbd "C-s-c") "org-agenda")
  (define-key *top-map* (kbd "s-x") "keybinding-s-x")
  (define-key *top-map* (kbd "s-X") "wi-project-ivy")
  (define-key *top-map* (kbd "s-i") "quassel-monitor")
  (mapcar #'(lambda (pair)
              (let ((command (concat "gmove-and-follow " (write-to-string (car pair))))
                    (key (concat "s-" (string (cdr pair)))))
                (define-key *top-map* (kbd key) command)))
          '((1 . #\!) (2 . #\@) (3 . #\#)
            (4 . #\$) (5 . #\%) (6 . #\^)
            (7 . #\&) (8 . #\*) (9 . #\()
            (0 . #\))))
  (define-key *top-map* (kbd "s-KP_Enter") "run-or-raise-xterm")
  (define-key *top-map* (kbd "s-k") "keybinding-s-k")
  (define-key *top-map* (kbd "s-K") "delete")
  (define-key *top-map* (kbd "s-ESC") "keybinding-s-k")
  (define-key *top-map* (kbd "s-Right") "move-focus right")
  (define-key *top-map* (kbd "s-Left") "move-focus left")
  (define-key *top-map* (kbd "s-Up") "move-focus up")
  (define-key *top-map* (kbd "s-Down") "move-focus down")
  (define-key *top-map* (kbd "s-Tab") "other-in-frame-or-fother")
  (define-key *top-map* (kbd "C-s-Tab") "fother")
  (define-key *top-map* (kbd "s-ISO_Left_Tab") "fother")
  (define-key *top-map* (kbd "S-s-Right") "move-window right")
  (define-key *top-map* (kbd "S-s-Left") "move-window left")
  (define-key *top-map* (kbd "S-s-Up") "move-window up")
  (define-key *top-map* (kbd "S-s-Down") "move-window down")
  (define-key *top-map* (kbd "s-v") "youtube-dl-music")
  (define-key *top-map* (kbd "s-V") "pulsemixer")
  (define-key *top-map* (kbd "s-c") "run-or-raise-xterm")
  (define-key *top-map* (kbd "s-F") "move-focus right")
  (define-key *top-map* (kbd "s-B") "move-focus left")
  (define-key *top-map* (kbd "s-P") "prev-in-frame")
  (define-key *top-map* (kbd "s-N") "next-in-frame")
  (define-key *top-map* (kbd "C-s-p") "prev-in-frame")
  (define-key *top-map* (kbd "C-s-n") "next-in-frame")
  (define-key *top-map* (kbd "s-S") "passmenu")
  (define-key *top-map* (kbd "s-s") "passmenu")
  (define-key *top-map* (kbd "s-;") "colon")
  (define-key *top-map* (kbd "s-[") "place-existing-windows")
  (define-key *top-map* (kbd "C-s-h") "sampler")
  (define-key *top-map* (kbd "C-M-S-s-RET") "xterm-dark-no-scrollbar")
  (define-key *top-map* (kbd "s-SunPrint_Screen") "zoom")
  (define-key *top-map* (kbd "s-b") "zoom")
  (define-key *top-map* (kbd "C-s-Right") "window-resize-by-half-horizontal")
  (define-key *top-map* (kbd "C-s-Left") "window-resize-by-half-vertical")

  (define-key *top-map* (kbd "C-M-s-S") "suspend")

  (define-key *top-map* (kbd "C-s-=") "volume-increase")
  (define-key *top-map* (kbd "C-s--") "volume-decrease")
  (define-key *top-map* (kbd "C-s-RET") "st")
  (define-key *top-map* (kbd "M-s-RET") "xfce-terminal")
  (define-key *top-map* (kbd "M-s-n") "mpv-next")
  (define-key *top-map* (kbd "M-s-p") "mpv-previous")

  (define-key *top-map* (kbd "M-s-e") "emacs-guix-edit")

  (define-key *top-map* (kbd "C-s-Up") "next-in-frame")
  (define-key *top-map* (kbd "C-s-Down") "prev-in-frame")

  (define-key *top-map* (kbd "M-s-d") "repology-guix-outdated")

  (define-key *top-map* (kbd "M-s-w") "ci-wigust")
  (define-key *top-map* (kbd "M-s-g") "ci-guix")

  (define-key *top-map* (kbd "SunPrint_Screen") "xfce-screenshooter")

  ;; Majordomo
  (define-key *top-map* (kbd "s-a") "mjru-pass-gitlab-ssh")
  (define-key *top-map* (kbd "s-A") "mjru-pass-eng")
  (define-key *top-map* (kbd "C-s-a") "kbd-C-s-a")
  (define-key *top-map* (kbd "s-u") "mjru-alerta")
  (define-key *top-map* (kbd "s-U") "mjru-cerb")
  (define-key *top-map* (kbd "M-s-u") "mjru-grafana-upstream-interfaces")
  (define-key *top-map* (kbd "C-s-u") "mjru-grafana-netflow")
  (define-key *top-map* (kbd "s-y") "greenclip")
  (define-key *top-map* (kbd "s-I") "s-I")

  ;; Rebind groups to PREFIX-NUMBER.
  (mapcar #'(lambda (x)
              ;; (define-key *top-map* (kbd (concat "s-" (write-to-string x)))
              ;;   (format nil "gselect ~D" x))
              (define-key *top-map* (kbd (concat "M-s-" (write-to-string x)))
                (format nil "group-~D-start-programs" x))
              (define-key *top-map* (kbd (concat "C-s-" (write-to-string x)))
                (format nil "~A ~D" "select-window-by-number" x)))
          (range 10 :min 0 :step 1))
  (define-key *top-map* (kbd (concat "s-1")) "group-1-start-programs")
  (define-key *top-map* (kbd (concat "s-2")) "group-2-start-programs")
  (define-key *top-map* (kbd (concat "s-3")) "group-3-start-programs")
  (define-key *top-map* (kbd (concat "s-4")) "group-4-start-programs")
  (define-key *top-map* (kbd (concat "s-5")) "group-5-start-programs")
  (define-key *top-map* (kbd (concat "s-6")) "group-6-start-programs")
  (define-key *top-map* (kbd (concat "s-7")) "group-7-start-programs")
  (define-key *top-map* (kbd (concat "s-8")) "group-8-start-programs")
  (define-key *top-map* (kbd (concat "s-9")) "group-9-start-programs")
  (define-key *top-map* (kbd (concat "s-0")) "gselect 0")

  (define-key *top-map* (kbd (concat "M-s-1")) "group-1-restart-programs")

  (define-key *top-map* (kbd (concat "s-Menu")) "xmenu"))

(define-key *root-map* (kbd "C-i") "set-prefix-key C-i")
(define-key *root-map* (kbd "C-t") "set-prefix-key C-t")

(define-key *root-map* (kbd "C-M-c") "run-or-raise-xterm")
(define-key *root-map* (kbd "C-c") "run-or-raise-xterm")
(define-key *root-map* (kbd "C-y") "greenclip")
(define-key *root-map* (kbd "Y") "xclip-kdeconnect-handler")
(define-key *root-map* (kbd "c") "run-or-raise-xterm")
(define-key *root-map* (kbd "e") "emacsclient")
(define-key *root-map* (kbd "quoteleft") "display-0-keys")
(define-key *root-map* (kbd "y") "greenclip")

(defcommand sxhkd-restart () ()
  (run-shell-command "sxhkd"))

(define-remapped-keys
    '(("(Firefox|Chrome)"
       ("C-n"   . "Down")
       ("C-p"   . "Up")
       ;; ("C-f"   . "Right")
       ;; ("C-b"   . "Left")
       ;; ("C-v"   . "Next")
       ;; ("M-v"   . "Prior")
       ;; ("M-w"   . "C-c")
       ;; ("C-w"   . "C-x")
       ;; ("C-y"   . "C-v")
       ;; ("M-<"   . "Home")
       ;; ("M->"   . "End")
       ;; ("C-M-b" . "M-Left")
       ;; ("C-M-f" . "M-Right")
       ;; ("C-k"   . ("C-S-End" "C-x"))
       )))
