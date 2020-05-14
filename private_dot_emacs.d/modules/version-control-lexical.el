;;; -*- lexical-binding: t; -*-

(defun wi-define-browse-url-git-commit (name directory url->commit)
  (defalias (intern (concat "browse-url-" name "-git-commit"))
    (function
     (lambda (url &optional new-window)
       (interactive (list (read-string "Commit: " nil nil (word-at-point))))
       (let ((default-directory directory)
             (commit (funcall url->commit url)))
         (magit-show-commit commit))))
    (format "Show a Git `commit' from the %s checkout.

If no commit hash provides, show a commit from hash at current point."
            name)))
