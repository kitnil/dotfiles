;;; -*- lexical-binding: t; -*-
(mapcar (lambda (directory)
          (let* ((parts (split-string directory "/"))
                 (name (first (last parts)))
                 (group (string-remove-prefix "_" (first (last (delete name parts)))))
                 (name+group (concat "majordomo-" group "-" name)))
            (defalias (intern (concat "browse-url-" name+group "-git-commit"))
              (function
               (lambda (url &optional new-window)
                 (interactive (list (read-string "Commit: " nil nil (word-at-point))))
                 (let ((default-directory directory)
                       (commit (funcall (lambda (url) (first (last (split-string url "/"))))
                                        url)))
                   (magit-show-commit commit))))
              (format "Show a Git `commit' from the %s checkout.

If no commit hash provides, show a commit from hash at current point."
                      name+group))))
        (wi-project-candidates-groups-direcotory))
