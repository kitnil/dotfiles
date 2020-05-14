;;; version-control-lexical.el --- Functions for version control -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Oleg Pykhalov <go.wigust@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((ffap "1.0") ((magit "1.0")))
;; Keywords: browse-url, magit, vc
;; URL: https://github.com/kitnil/dotfiles/

;;; Commentary:

;; This package provides functions to work with version control systems.

;;; Code:

(require 'ffap)
(require 'magit-diff)

;;;###autoload
(defun wi-define-browse-url-git-commit (name directory url->commit)
  "Define browse-url-* function.

NAME part of function name browse-url-NAME-git-commit.
DIRECTORY Git project directory.
URL->COMMIT function to parse commit from url.

The following example defines `browse-url-guix-git-commit'
function, which opens
https://git.savannah.gnu.org/cgit/guix.git/commit/?id=df05842332be80e…
with `magit-show-commit' function in ~/src/guix directory.

\(wi-define-browse-url-git-commit
 \"guix\"
 (expand-file-name \"~/src/guix\")
 (lambda (url) (car (last (split-string url \"=\")))))"
  (defalias (intern (concat "browse-url-" name "-git-commit"))
    (function
     (lambda (url &optional _new-window)
       (interactive (list (read-string "Commit: " nil nil (word-at-point))))
       (let ((default-directory directory)
             (commit (funcall url->commit url)))
         (magit-show-commit commit))))
    (format "Show a Git `commit' from the %s checkout.

If no commit hash provides, show a commit from hash at current point."
            name)))

;;; version-control-lexical.el ends here
