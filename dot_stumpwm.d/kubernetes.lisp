(in-package :stumpwm)

(defvar *kubernetes-current-cluster* "")

(defcommand kubernetes-update-current-cluster () ()
  (setq *kubernetes-current-cluster*
        (string-trim
         '(#\Newline)
         (run-shell-command (join (list "yq" "--raw-output" "'.\"current-context\"'" (concat (getenv "HOME") "/.kube/config")))
                            t))))

(defcommand kubernetes-update-current-cluster-inotify () ()
  (run-shell-command
   (join
    (list "ls" "-1" (concat (getenv "HOME") "/.kube/config")
          "|" "entr" "-n" "-r" "bash" "-c"
          (format nil "~s" "echo '(kubernetes-update-current-cluster)' | stumpish -e eval")))))

(defun kubectl-current-context (cluster &optional color)
  "Returns a string representing the current kubectl context."
  (if color
      (cond ((string= cluster "bq-k8s-stag")
             (format nil "kubernetes: ^[^B^3*~a^]" cluster))
            ((string= cluster "bq-k8s-prod")
             (format nil "kubernetes: ^[^B^1*~a^]" cluster))
            (t
             (format nil "kubernetes: ~a" cluster)))
      (format nil "kubernetes: ~a" cluster)))
