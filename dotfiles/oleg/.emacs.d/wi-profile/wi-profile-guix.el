;;; wi-profile-guix.el --- Emacs Guix profile        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Oleg Pykhalov

;; Author: Oleg Pykhalov <oleg@guixsd>
;; Keywords: internal, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides Emacs profile for Guix hacking.

;;; Code:

(require 'beginend)
(require 'geiser)
(require 'git-gutter)
(require 'magit)
(require 'projectile)
(require 'yasnippet)

(defvar wi-elisp--prettify-symbols-alist
  '(("lambda" . ?λ)
    ("lambda*" . (?λ (Br . Bl) ?*)))
  "Alist of symbol prettifications for `emacs-lisp-mode'.")

(setq enable-local-variables :all)

(global-set-key (kbd "<f5>") #'projectile-run-shell)
(global-set-key (kbd "<f6>") #'magit-status)
(global-set-key (kbd "<f7>") #'yas-insert-snippet)

(beginend-global-mode)
(global-git-gutter-mode)
(projectile-global-mode)
(yas-global-mode)

(add-hook 'find-file-hook 'auto-insert)
(add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (set (make-local-variable 'prettify-symbols-alist)
                 wi-elisp--prettify-symbols-alist)))
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(provide 'wi-profile-guix)
;;; wi-profile-guix.el ends here
