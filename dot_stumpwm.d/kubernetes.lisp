(in-package :stumpwm)

(defvar *kubernetes-current-cluster* "")

(defcommand kubernetes-update-current-cluster () ()
  (setq *kubernetes-current-cluster*
        (string-trim
         '(#\Newline)
         (run-shell-command (join (list "yq" "--raw-output" "'.\"current-context\"'" (concat (getenv "HOME") "/.kube/config")))
                            t))))
