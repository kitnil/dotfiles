;; Copyright © 2018, 2019, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(in-package :stumpwm)

(require "asdf")

(defun load-config-file (file)
  (load (concat (getenv "HOME") "/.stumpwm.d/" file)))

(load-config-file "utils.lisp")

;; Tuesday January 3 2005 23:05:25
(setq *time-format-string-default* "%A %B %e %Y %k:%M:%S")

(load-config-file "keys.lisp")
(load-config-file "nav.lisp")
(load-config-file "theme.lisp")
(load-config-file "xorg.lisp")
(load-config-file "term.lisp")
(load-config-file "text-editors.lisp")
(load-config-file "repl.lisp")

(load-config-file "notify.lisp")
(load-config-file "hardware.lisp")
(load-config-file "admin.lisp")
(load-config-file "clipboard.lisp")
(load-config-file "screenshoot.lisp")
(load-config-file "password.lisp")
(load-config-file "trans.lisp")
(load-config-file "backup.lisp")
(load-config-file "documentation.lisp")
(load-config-file "emacs.lisp")
(load-config-file "chat.lisp")
(load-config-file "mail.lisp")
(load-config-file "docker.lisp")
(load-config-file "vm.lisp")
(load-config-file "vnc.lisp")
(load-config-file "rofi.lisp")
(load-config-file "audio.lisp")
(load-config-file "mpv.lisp")
(load-config-file "streamlink.lisp")
(load-config-file "youtube-dl.lisp")
(load-config-file "android.lisp")
(load-config-file "kodi.lisp")
(load-config-file "web.lisp")
(load-config-file "majordomo.lisp")

(load-config-file "disk.lisp")
(load-config-file "torrent.lisp")
(load-config-file "rest.lisp")
(load-config-file "cpu.lisp")
(load-config-file "mem.lisp")
(load-config-file "imap.lisp")
(load-config-file "covid19.lisp")
(load-config-file "spb.lisp")
(load-config-file "mode-line.lisp")

;; (load-config-file "gaps.lisp")

(defvar workstation?
  (string-equal (file-get-contents "/etc/hostname") "workstation-guixsd"))
(cond ((string= (getenv "DISPLAY") ":0")
       (load-config-file "display-0.lisp")
       (unless workstation?
         (setf *maxsize-border-width* 3)
         (setf *message-window-y-padding* 3)
         (setf *normal-border-width* 3)
         (setf *transient-border-width* 3)))
      ((string= (getenv "DISPLAY") ":1")
       (run-shell-command "xsetroot -solid grey")
       (restore-from-file (concat (getenv "HOME") "/src/dotfiles/oleg/.stumpwm.d/desktop/1.lisp"))
       ;; (swm-gaps:toggle-gaps) ;XXX: Make declarative.
       (bind-super)
       (define-key *top-map* (kbd "s-m") "alerta")
       (setf *maxsize-border-width* 3)
       (setf *message-window-y-padding* 3)
       (setf *normal-border-width* 3)
       (setf *transient-border-width* 3)
       (frame-parameters-display-1))
      (t (set-prefix-key (kbd "C-i"))))

(setf *startup-message* nil)
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

(load-config-file "autostart.lisp")
