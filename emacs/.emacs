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

(setq initial-buffer-choice t)

(setq mail-user-agent 'gnus-user-agent)

(setq visible-bell t)

(load-theme 'manoj-dark)

(custom-theme-set-faces
 'manoj-dark
 '(fringe ((t (:background "black" :foreground "Wheat"))))
 '(header-line
   ((t (:background "black" :foreground "grey90" :height 0.9))))
 '(magit-diff-added ((t (:inherit diff-added))))
 '(magit-diff-added-highlight ((t (:inherit diff-added :background "grey10"))))
 '(magit-diff-context-highlight ((t (:background "grey10"))))
 '(magit-diff-hunk-heading ((t (:inherit diff-hunk-header))))
 '(magit-diff-hunk-heading-highlight ((t (:inherit diff-hunk-header))))
 '(magit-diff-removed ((t (:inherit diff-removed))))
 '(magit-diff-removed-highlight ((t (:inherit diff-removed :background "grey10"))))
 '(which-key-command-description-face ((t (:inherit font-lock-function-name-face :height 1.0)))))

(with-eval-after-load 'whitespace
  (setq whitespace-style (quote (face tabs spaces trailing
                                      space-before-tab newline
                                      indentation empty
                                      space-after-tab space-mark
                                      tab-mark)))

  (let ((foreground "gray15"))
    (mapc (lambda (font)
            (set-face-attribute font nil
                                :background nil :foreground foreground))
          '(whitespace-space whitespace-indentation))))

(add-hook 'prog-mode-hook 'whitespace-mode)


;;;
;;; Enable functions
;;;

(put 'narrow-to-region 'disabled nil)


;;;
;;; Keybindings
;;;
;;; See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html>

(which-key-add-key-based-replacements "C-c v" "magit")
(global-set-key (kbd "C-c v l") #'magit-list-repositories)
(global-set-key (kbd "C-c v s") #'magit-status)

(which-key-add-key-based-replacements "C-c f" "point")
(global-set-key (kbd "C-c f e") #'guix-edit)
(global-set-key (kbd "C-c f f") #'ffap)

(which-key-add-key-based-replacements "C-c t" "toggle")
(global-set-key (kbd "C-c t i") #'aggressive-indent-mode)
(global-set-key (kbd "C-c t t") #'toggle-truncate-lines)
(global-set-key (kbd "C-c t s") #'flyspell-mode)
(global-set-key (kbd "C-c t c") #'flycheck-mode)
(global-set-key (kbd "C-c t m") #'flymake-mode)
(global-set-key (kbd "C-c t w") #'whitespace-mode)
(global-set-key (kbd "C-c t p") #'smartparens-global-mode)
(global-set-key (kbd "C-c t l") #'prettify-symbols-mode)
(global-set-key (kbd "C-c t g") #'guix-prettify-mode)
(global-set-key (kbd "C-c t h") #'hl-line-mode)

(which-key-add-key-based-replacements "C-c r" "rething")
(global-set-key (kbd "C-c r r") #'revert-buffer)
(global-set-key (kbd "C-c r l") #'redraw-display)

(which-key-add-key-based-replacements "C-c h" "helm")
(global-set-key (kbd "C-c h i") #'helm-imenu)
(global-set-key (kbd "C-c h s") #'helm-pass)
(global-set-key (kbd "C-c h y") #'helm-show-kill-ring)

(which-key-add-key-based-replacements "C-c h p" "helm-projectile")
(global-set-key (kbd "C-c h p p") #'helm-projectile)
(global-set-key (kbd "C-c h p f") #'helm-projectile-find-file-dwim)
(global-set-key (kbd "C-c h w") #'helm-stumpwm-commands)

(which-key-add-key-based-replacements "C-c p x" "projectile-shell")
(which-key-add-key-based-replacements "C-c p s" "projectile-search")

(which-key-add-key-based-replacements "C-c m" "mail")
(global-set-key (kbd "C-c m b") #'wi-send-buffer-as-mail)

(which-key-add-key-based-replacements "C-c m g" "gnus")
(global-set-key (kbd "C-c m g g") #'gnus)
(global-set-key (kbd "C-c m g s") #'switch-to-gnus)

(which-key-add-key-based-replacements "C-c s" "shell")
(global-set-key (kbd "C-c s s") #'shell)
(global-set-key (kbd "C-c s c") #'compilation-shell-minor-mode)
(global-set-key (kbd "C-c s e") #'eshell)

(which-key-add-key-based-replacements "C-c c" "org")
(global-set-key (kbd "C-c c c") #'org-capture)
(global-set-key (kbd "C-c c a") #'org-agenda)
(global-set-key (kbd "C-c c a") #'org-store-link)

(which-key-add-key-based-replacements "C-c &" "yasnippet")


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

(setq debpaste-user-name "wigust")

(pdf-tools-install)

(with-eval-after-load 'eww
  (setq shr-width 80)
  (setq shr-use-fonts nil))

(show-paren-mode)

(setq projectile-completion-system 'default)
(projectile-global-mode)

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

(setq browse-url-mpv-program "mpv")
(setq browse-url-mpv-arguments nil)
(setq browse-url-mpv-remote-program "~/bin/mpv-remote")
(defun browse-url-mpv (url &optional new-window)
  "Ask the mpv video player to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-mpv-arguments' to mpv."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "mpv " url) nil
           browse-url-mpv-program
           (append
            browse-url-mpv-arguments
            (list url)))))

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
;; (with-eval-after-load 'info
;;   (info-initialize)
;;   (setq Info-directory-list
;;         (append (wi-expand-file-names (list "~/src/guix/doc"))
;;                 Info-directory-list)))
;;
;; Alternative: https://lists.gnu.org/archive/html/help-guix/2017-03/msg00140.html
;; See <~/.bashrc>

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
  (add-hook 'minibuffer-inactive-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'scheme-mode-hook 'smartparens-strict-mode))

(winner-mode 1)
(windmove-default-keybindings)

(which-key-mode)
(which-key-setup-side-window-right)

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
