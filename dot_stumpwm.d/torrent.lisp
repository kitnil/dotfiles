(in-package :stumpwm)

(defvar *transmission-hostname* "magnolia")

(defvar *torrent-seeds-counter* 0)

(defun torrent-seeds ()
  (length (filter (lambda (str)
                    (string-contains "Seeding" str))
                  (split-string (run-shell-command "transmission-remote --list" t)
                                '(#\newline)))))

(defcommand torrent-seeds-update-counter () ()
  (setq *torrent-seeds-counter* (torrent-seeds)))
