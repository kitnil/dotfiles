;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(setq user-mail-address "go.wigust@gmail.com")
(setq user-full-name    "Oleg Pykhalov")

(when (display-graphic-p)
  (load-theme 'manoj-dark))


;;;
;;; Keybindings
;;;

(global-set-key (kbd "C-c f f") #'ffap)
(global-set-key (kbd "C-c f p") #'guix-edit)
(global-set-key (kbd "C-c b b") #'ibuffer)
(global-set-key (kbd "C-c v s") #'magit-status)


;;;
;;; General functions for use
;;;

(defun wi-expand-file-names (files)
    (mapcar (lambda (file) (expand-file-name file))
            files))


;;;
;;; URLs for browse-url functions
;;;

(setq browse-url-browser-function
      `(("^ftp://.*" . browse-ftp-tramp)
        ("^https?://debbugs\\.gnu\\.org/.*" . debbugs-browse-url)
        ("^https?://w*\\.?youtube.com/watch\\?v=.*" . browse-url-mpv)
        ("^https?://w*\\.?youtube.com/.*" . browse-url-chromium)
        ("^https?://w*\\.?github.com/.*" . browse-url-chromium)
        ("." . browse-url-firefox)))


;;;
;;; Guile and Guix
;;;

(with-eval-after-load 'geiser
  (setq geiser-active-implementations (quote (guile))))

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "/home/natsu/src/guix")
  (setq geiser-guile-binary '("guile" "--no-auto-compile")))

(add-hook 'scheme-mode-hook 'guix-devel-mode)
(add-hook 'proced-post-display-hook 'guix-prettify-mode)
(add-hook 'shell-mode-hook #'guix-prettify-mode)
(add-hook 'dired-mode-hook 'guix-prettify-mode)

(with-eval-after-load 'guix-repl
  (setq guix-directory "~/src/guix"))

(setq guix-read-package-name-function #'guix-read-package-name-at-point)


;;;
;;; winner-undo and winner-redo
;;;

(winner-mode 1)
(windmove-default-keybindings)


;;;
;;; C-mode
;;;

(with-eval-after-load 'cc-mode
  (defconst c--prettify-symbols-alist
    '(("->"     . ?→)
      ("=="     . ?≡)
      ("not"    . ?¬)
      ("&&"     . ?∧)
      ("||"     . ?∨)
      ("!="     . ?≠)
      ("<="     . ?≤)
      (">="     . ?≥)
      ("true"  . ?T)
      ("false" . ?F)))

  (add-hook 'c-mode-hook (lambda ()
                           (set (make-local-variable 'prettify-symbols-alist)
                                c--prettify-symbols-alist)))

  (add-hook 'c-mode-hook #'prettify-symbols-mode))

(with-eval-after-load 'cc-vars
  (add-to-list 'c-cleanup-list 'space-before-funcall))

(with-eval-after-load 'semantic
  (global-semantic-decoration-mode t))

;;; Complition framework.  See <https://company-mode.github.io/>

(with-eval-after-load 'company
  (setq company-clang-insert-arguments nil)
  (setq company-gtags-insert-arguments nil)
  (setq company-semantic-insert-arguments nil))


;;;
;;; Structured editing.  See <https://github.com/bbatsov/smartparens>
;;;

(smartparens-global-mode)
(require 'smartparens-config)


;;;
;;; Snippets
;;;

(setq yas-snippet-dirs (list "~/.emacs.d/snippets"
                             "~/.guix-profile/share/emacs/yasnippet-snippets/"))
(yas-global-mode)
(yas-reload-all)


;;;
;;; Multiple cursors
;;;
;;; More usable then default `mouse-buffer-menu' on mouse.

(multiple-cursors-mode)
(global-set-key (kbd "<C-down-mouse-1>") 'mc/toggle-cursor-on-click)


;;;
;;; Documentation
;;;

;; Code from: https://github.com/alezost/guix.el/pull/9#issuecomment-340556583
(with-eval-after-load 'info
  (info-initialize)
  (setq Info-directory-list
        (append (wi-expand-file-names (list "~/src/guile-chickadee/doc"
                                            "~/src/guix/doc"
                                            "~/src/stumpwm"))
                Info-directory-list)))

(which-key-mode)

(with-eval-after-load 'debbugs-gnu
  (setq debbugs-gnu-default-packages (list "guix" "guix-patches")))

(with-eval-after-load 'sendmail
  (setq send-mail-function #'smtpmail-send-it)
  (setq smtpmail-smtp-server "smtp.gmail.com"))

;; (add-hook 'scheme-mode-hook #'geiser-mode)

(defun debbugs-gnu-guix ()
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") '("guix")))

(defun debbugs-gnu-guix-patches ()
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") '("guix-patches")))

(defun set-current-frame-80-40 ()
  (interactive)
  (set-frame-size (selected-frame) 80 40))

(defun set-current-frame-80-24 ()
  (interactive)
  (set-frame-size (selected-frame) 80 24))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(safe-local-variable-values
   (quote
    ((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (bug-reference-bug-regexp . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
