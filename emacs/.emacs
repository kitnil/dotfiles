;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

;; Tip: "M-x e" on `(emacs-init-time)'.

(setq load-prefer-newer t)

(setq user-mail-address    "go.wigust@gmail.com")
(setq user-full-name       "Oleg Pykhalov")
(setq default-input-method "russian-computer")

(setq display-time-24hr-format t)
(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)

(setq mail-user-agent 'gnus-user-agent)

(when (display-graphic-p)
  (load-theme 'manoj-dark)
  (setq visible-bell t))


;;;
;;; Enable functions
;;;

(put 'narrow-to-region 'disabled nil)


;;;
;;; Keybindings
;;;

(global-set-key (kbd "C-c d") #'magit-list-repositories)
(global-set-key (kbd "C-c e") #'guix-edit)
(global-set-key (kbd "C-c f") #'ffap)
(global-set-key (kbd "C-c l") #'redraw-display)
(global-set-key (kbd "C-c p") #'projectile-find-file)
(global-set-key (kbd "C-c s") #'magit-status)


;;;
;;; Usability functions
;;;

(defun wi-delete-current-buffer-file ()
  "Delete the current buffer and the file connected with it"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (yes-or-no-p "Are you sure, want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun wi-send-buffer-as-mail ()
  (interactive)
  (let ((str (buffer-string)))
    (compose-mail)
    (save-excursion
      (message-goto-body)
      (insert str))))

(defun wi-turn-on-truncate-lines ()
  "Convenience method to turn on `truncate-lines'."
  (interactive)
  (toggle-truncate-lines 1))


;;;
;;; General functions for use
;;;

(defun wi-expand-file-names (files)
    (mapcar (lambda (file) (expand-file-name file))
            files))

(defun wi-list-files-in-dir (directory)
  "Return a list of files in the DIRECTORY."
  (directory-files (expand-file-name directory)
                   t
                   "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))


;;;
;;; Elisp
;;;

(with-eval-after-load 'elisp-mode
  (defconst wi-elisp--prettify-symbols-alist
    '(("lambda"  . ?λ)
      ("lambda*" . (?λ (Br . Bl) ?*))
      ("not"     . ?¬)
      ("and"     . ?∧)
      ("or"      . ?∨)
      ("eq?"     . ≡)
      ("<="      . ?≤)
      (">="      . ?≥)
      ("->"      . ?→)))
  (add-hook 'c-mode-hook (lambda ()
                           (set (make-local-variable 'prettify-symbols-alist)
                                wi-elisp--prettify-symbols-alist)))
  (add-hook 'c-mode-hook #'prettify-symbols-mode))


;;;
;;; Guile and Guix
;;;

(with-eval-after-load 'geiser
  (setq geiser-active-implementations (quote (guile))))

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "/home/natsu/src/guix")
  (setq geiser-guile-binary '("guile" "--no-auto-compile")))

(with-eval-after-load 'scheme
  (add-hook 'scheme-mode-hook 'guix-devel-mode)
  (defconst wi-scheme--prettify-symbols-alist
    '(("lambda"  . ?λ)
      ("lambda*" . (?λ (Br . Bl) ?*))
      ("#t"      . ?T)
      ("#f"      . ?F)
      ("not"     . ?¬)
      ("and"     . ?∧)
      ("or"      . ?∨)
      ("eq?"     . ≡)
      ("<="      . ?≤)
      (">="      . ?≥)
      ("->"      . ?→)))
  (add-hook 'c-mode-hook (lambda ()
                           (set (make-local-variable 'prettify-symbols-alist)
                                wi-scheme--prettify-symbols-alist)))

  (add-hook 'c-mode-hook #'prettify-symbols-mode))

(add-hook 'proced-post-display-hook 'guix-prettify-mode)
(add-hook 'shell-mode-hook #'guix-prettify-mode)
(add-hook 'dired-mode-hook 'guix-prettify-mode)

(with-eval-after-load 'guix-repl
  (setq guix-directory "~/src/guix"))

(setq guix-read-package-name-function #'guix-read-package-name-at-point)


;;;
;;; C-mode
;;;

(with-eval-after-load 'cc-mode
  (defconst wi-c--prettify-symbols-alist
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
                                wi-c--prettify-symbols-alist)))

  (add-hook 'c-mode-hook #'prettify-symbols-mode))

(with-eval-after-load 'cc-vars
  (add-to-list 'c-cleanup-list 'space-before-funcall))

(with-eval-after-load 'semantic
  (global-semantic-decoration-mode t))


;;;
;;; Magit
;;;

(defvar wi-projects-directory "/srv/git")

(defun wi-update-magit-repository-directories (directory)
  "Update list of files in `DIRECTORY' for `magit-list-repositories'."
  (interactive)
  (setq magit-repository-directories (wi-list-files-in-dir directory)))

(wi-update-magit-repository-directories wi-projects-directory)

(setq magit-log-arguments (list "--graph" "--color" "--decorate" "-n64"))
(setq magit-log-section-arguments (list "-n256" "--decorate"))

;; Use `magit-describe-section'
(defun wi-local-magit-initially-hide-unmerged (section)
  (and (not magit-insert-section--oldroot)
       (or (eq (magit-section-type section) 'unpushed)
           (equal (magit-section-value section) "@{upstream}..")
           (eq (magit-section-type section) 'stashes)
           (equal (magit-section-value section) "refs/stash"))
       'hide))

(add-hook 'magit-section-set-visibility-hook
          'wi-local-magit-initially-hide-unmerged)



;;;
;;; Misc
;;;

(show-paren-mode)

(setq projectile-completion-system 'default)

(setq helm-locate-project-list (wi-list-files-in-dir wi-projects-directory))

(require 'google-translate-mode)
(with-eval-after-load 'google-translate-mode
  (setq trans-target "ru"))

(with-eval-after-load 'time
  (setq display-time-world-time-format "%Z\t%d %B %H:%M")
  (setq display-time-world-list '(("Europe/Moscow"    "Europe/Moscow")
                                  ("Europe/Berlin"    "Europe/Berlin")
                                  ("Europe/London"    "Europe/London")
                                  ("Europe/Istanbul"  "Europe/Istanbul")
                                  ("America/Winnipeg" "America/Winnipeg")
                                  ("America/New_York" "America/New_York")
                                  ("Asia/Tokyo"       "Asia/Tokyo"))))

(setq gitpatch-mail-database (list "guix-patches@gnu.org"))

(save-place-mode)            ; Remember position in files
(setq mouse-yank-at-point t) ; Ignore mouse position on paste
(setq vc-follow-symlinks t)  ; Do not ask about following link in Git projects
(setq dired-listing-switches (purecopy "-alh")) ; Prettify dired

(global-prettify-symbols-mode)
(setq prettify-symbols-unprettify-at-point 'right-edge)

(setq browse-url-browser-function
      `(("^ftp://.*" . browse-ftp-tramp)
        ("^https?://debbugs\\.gnu\\.org/.*" . debbugs-browse-url)
        ("^https?://w*\\.?youtube.com/watch\\?v=.*" . browse-url-mpv)
        ("^https?://w*\\.?youtube.com/.*" . browse-url-chromium)
        ("^https?://w*\\.?github.com/.*" . browse-url-chromium)
        ("." . browse-url-firefox)))

(with-eval-after-load 'sendmail
  (setq send-mail-function #'smtpmail-send-it)
  (setq smtpmail-smtp-server "smtp.gmail.com"))

;; Code from: https://github.com/alezost/guix.el/pull/9#issuecomment-340556583
(with-eval-after-load 'info
  (info-initialize)
  (setq Info-directory-list
        (append (wi-expand-file-names (list "~/src/guile-chickadee/doc"
                                            "~/src/guix/doc"
                                            "~/src/stumpwm"))
                Info-directory-list)))

(multiple-cursors-mode)
(global-set-key (kbd "<C-down-mouse-1>") 'mc/toggle-cursor-on-click)

(setq yas-snippet-dirs (list "~/.emacs.d/snippets"
                             "~/.guix-profile/share/emacs/yasnippet-snippets/"))
(yas-global-mode)
(yas-reload-all)

(with-eval-after-load 'company
  (setq company-clang-insert-arguments nil)
  (setq company-gtags-insert-arguments nil)
  (setq company-semantic-insert-arguments nil))

(smartparens-global-mode)
(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (add-hook 'minibuffer-inactive-mode-hook 'smartparens-mode))

(winner-mode 1)
(windmove-default-keybindings)

(which-key-mode)

(setq helm-firefox-default-directory "~/.mozilla/icecat/")
(setq ewmctrl-wmctrl-path "~/.guix-profile/bin/wmctrl")

(with-eval-after-load 'debbugs-gnu
  (setq debbugs-gnu-default-packages (list "guix" "guix-patches")))

(defun wi-debbugs-gnu-guix ()
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") '("guix")))

(defun wi-debbugs-gnu-guix-patches ()
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") '("guix-patches")))

(defun wi-set-current-frame-80-40 ()
  (interactive)
  (set-frame-size (selected-frame) 80 40))

(defun wi-set-current-frame-80-24 ()
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
