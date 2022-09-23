;; Copyright © 2018, 2019, 2020, 2021, 2022 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(in-package :stumpwm)

(require "asdf")

;; https://discourse.nixos.org/t/fonts-in-nix-installed-packages-on-a-non-nixos-system/5871/9
(defvar *fontconfig-file*
  "FONTCONFIG_FILE=/run/current-system/profile/etc/fonts/fonts.conf")

(defun load-config-file (file)
  (load (concat (getenv "HOME") "/.stumpwm.d/" file)))

(load-config-file "utils.lisp")

(defcommand quassel () ()
            (run-shell-command (join (list *fontconfig-file* "/home/oleg/.nix-profile/bin/quassel"))))

;; Tuesday January 3 2005 23:05:25
(setq *time-format-string-default* "%A %B %e %Y %k:%M:%S")

(cond ((or (string= (getenv "DISPLAY") ":0")
           (string= (getenv "DISPLAY") ":1")
           (string= (getenv "DISPLAY") ":2")
           (string= (getenv "DISPLAY") ":3"))
       (redirect-all-output
        (concat
         (getenv "HOME") "/.local/var/log/stumpwm/" (getenv "DISPLAY") ".log"))

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
       (load-config-file "vnc.lisp")
       (load-config-file "rofi.lisp")
       (load-config-file "audio.lisp")
       (load-config-file "mpv.lisp")
       (load-config-file "streamlink.lisp")
       (load-config-file "youtube-dl.lisp")
       (load-config-file "android.lisp")
       (load-config-file "kodi.lisp")
       (load-config-file "web.lisp")
       (load-config-file "time.lisp")
       (load-config-file "mjru.lisp")
       (load-config-file "virtualization.lisp")
       (load-config-file "bittorrent.lisp")

       (load-config-file "kubernetes.lisp")
       (load-config-file "disk.lisp")
       (load-config-file "rest.lisp")
       (load-config-file "cpu.lisp")
       (load-config-file "mem.lisp")
       (load-config-file "imap.lisp")
       (load-config-file "covid19.lisp")
       (load-config-file "gpg.lisp")
       (load-config-file "vpn.lisp")
       (load-config-file "mode-line.lisp")
       (load-config-file "display-0.lisp")

       (setf *startup-message* nil)
       (setf *message-window-gravity* :center)
       (setf *input-window-gravity* :center)

       (load-config-file "autostart.lisp")
       (load-config-file "swank.lisp")
       (load-config-file "gaps.lisp"))
      ((string= (getenv "DISPLAY") ":10")
       (add-hook *start-hook* (lambda () (quassel)))))

(when (string= (file-get-contents "/etc/hostname") "gnu")
  (run-shell-command (join (list "xterm" "-bg" "black" "-fg" "white" "-fa" "Monospace" "-fs" "11"))))

;; "sudo herd status; exec bash"
