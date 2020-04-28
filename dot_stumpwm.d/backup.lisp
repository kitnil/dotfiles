(in-package :stumpwm)

(defun xpanes-command (command)
  (format nil "xpanes -t -c ~s" command))

(defcommand xpanes-restic-snapshots () ()
  (term-shell-command (join `(,(xpanes-command (join `("sudo" "-i" ,(format nil "RESTIC_PASSWORD=~a"
                                                                            (password-store-show "wugi.info/restic/all"))
                                                              ,(format nil "~a/.guix-profile/bin/restic" (getenv "HOME"))
                                                              "-r" "/srv/backup/{}" "snapshots")))
                               ,@'("guixsd" "majordomo" "oracle" "spb")))
                      :title "xpanes-restic-snapshots"))

(defcommand-alias restic-snapshots xpanes-restic-snapshots)

