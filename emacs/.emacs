;;; .emacs --- user-init-file

;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

;;; Commentary:
;;
;; GNU Emacs configuration file.

;;; Code:

(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))

(setq load-prefer-newer t)

(setq user-mail-address "go.wigust@gmail.com")
(setq user-full-name "Oleg Pykhalov")

(setq inhibit-compacting-font-caches t)

(defun find-user-init-file ()
  "Edit this config."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c e c") 'find-user-init-file)

(defun me-update-my-projects ()
  (interactive)
  (setq my-projects
        (directory-files (expand-file-name "/srv/git")
                         t
                         "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(me-update-my-projects)


;;;
;;; Custom set variables
;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(debbugs-gnu-default-packages (quote ("guix" "guix-patches")))
 '(default-input-method "russian-computer")
 '(geiser-guile-binary "guile")
 '(gitpatch-mail-database (quote ("guix-patches@gnu.org")))
 '(ido-mode (quote buffer) nil (ido))
 '(imaxima-scale-factor 1.5)
 '(indent-tabs-mode nil)
 '(magit-auto-revert-mode nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(magit-log-section-arguments (quote ("-n256" "--decorate")))
 '(mouse-yank-at-point t)
 '(nnir-notmuch-remove-prefix "/home/natsu/Maildir/")
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "today" :query "date:today")
     (:name "leti-user" :query "from:leti-user@googlegroups.com tag:unread")
     (:name "patch-today" :query "subject:patch date:today")
     (:name "youtube" :query "from:noreply@youtube.com")
     (:name "youtube-today" :query "from:noreply@youtube.com date:today")
     (:name "youtube-unread" :query "from:noreply@youtube.com tag:unread")
     (:name "headhunter-unread" :query "from:no_reply@hh.ru tag:unread")
     (:name "artgames" :query "tag:unread subject:ArtGames LP")
     (:name "my-email" :query "from:go.wigust@gmail.com"))))
 '(org-agenda-files nil)
 '(safe-local-variable-values
   (quote
    ((eval add-to-list
           (quote geiser-guile-load-path)
           (expand-file-name "."))
     (bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")
     (rainbow-identifiers-mode)
     (eval setq-default truncate-lines 1)
     (eval add-hook
           (quote compilation-mode-hook)
           (lambda nil
             (setq-local truncate-lines 1)))
     (eval add-hook
           (quote shell-mode-hook)
           (quote guix-build-log-minor-mode))
     (eval add-hook
           (quote compilation-mode-hook)
           (quote guix-build-log-minor-mode))
     (aggressive-indent-mode)
     (Base . 10)
     (Syntax . Common-Lisp)
     (Package . Maxima)
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (bug-reference-bug-regexp . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>"))))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(sp-base-key-bindings (quote sp))
 '(tab-always-indent (quote complete))
 '(tool-bar-mode nil)
 '(trans-target "ru")
 '(vc-follow-symlinks t)
 '(x-underline-at-descent-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 143 :width normal))))
 '(sml/filename ((t (:inherit sml/global :foreground "red" :weight normal))))
 '(sml/line-number ((t (:inherit sml/modes :weight normal))))
 '(sml/modified ((t (:inherit sml/not-modified :foreground "black" :weight normal))))
 '(sml/position-percentage ((t (:foreground "black"))))
 '(sml/process ((t (:foreground "red"))))
 '(sml/read-only ((t (:inherit sml/not-modified :foreground "black" :weight normal)))))
(put 'narrow-to-region 'disabled nil)


;;;
;;; Highlight
;;;

(use-package hi-lock
  :config
  (progn
    (setq highlight-words-list
          '(("proced" . "font-lock-function-name-face")
            ("expres" . "bold")
            ("function" . "font-lock-function-name-face")
            ("case" . "font-lock-keyword-face")
            ("symbol" . "bold")
            ("compound" . "font-lock-function-name-face")
            ("condition" . "font-lock-keyword-face")
            ("predicate" . "bold")
            ("value" . "bold")
            ("decompos" . "font-lock-function-name-face")
            ("define" . "font-lock-keyword-face")))
    (defun me-info-highlight-words
        (lambda ()
          (mapcar (lambda (word-font)
                    (font-lock-add-keywords
                     nil `(,(concat "\\<\\(" (car word-font) "\\)") 1
                           ,(cdr word-font) t)))
                  highlight-words-list)))))

(use-package rainbow-mode
  :bind (("C-c t r" . rainbow-mode)))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package hl-todo
  :config
  (progn
    (add-hook 'latex-mode-hook 'hl-todo-mode)
    (add-hook 'prog-mode-hook 'hl-todo-mode)))


;;;
;;; Built in
;;;

(add-hook 'occur-mode-hook (lambda () (setq-local truncate-lines t)))

(use-package simple
  :bind (("C-c t l" . toggle-truncate-lines)))

(use-package scroll-lock
  :bind (("<Scroll_Lock>" . scroll-lock-mode)))

(use-package mwheel
  :bind (("<C-mouse-4>" . text-scale-increase)
         ("<C-mouse-5>" . text-scale-decrease)))

(use-package menu-bar
  :preface
  (menu-bar-mode 0))

(use-package dired
  :commands dired-mode
  :config
  (progn
    (defun my-dired-mode-hook ()
      (turn-on-gnus-dired-mode)
      (dired-async-mode t)
      (setq-local truncate-lines t))
    (add-hook 'dired-mode-hook 'my-dired-mode-hook)
    (with-eval-after-load 'dired-x
      (mapcar (lambda (extension)
                (add-to-list 'dired-guess-shell-alist-user
                             `(,extension
                               ,"mpv --no-resume-playback --keep-open=no")))
              '("\\.mp4$" "\\.webm$")))))

(use-package ibuffer
  :bind (("C-c b i" . ibuffer)
         ("C-c b s" . scratch))
  :preface
  (defun scratch ()
    (interactive)
    (let ((current-mode major-mode))
      (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (goto-char (point-min))
      (when (looking-at ";")
        (forward-line 4)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (lisp-interaction-mode))))

(use-package diff-mode
  :config
  (progn
    (defun my-diff-mode-hook ()
      (setq-local truncate-lines t)
      (setq ediff-window-setup-function 'ediff-setup-windows-default))
    (defun ediff-setup-windows-default (buffer-A buffer-B buffer-C control-buffer)
      (funcall 'ediff-setup-windows-plain
	       buffer-A buffer-B buffer-C control-buffer))
    (add-hook 'diff-mode-hook 'my-diff-mode-hook)))

(use-package compile
  :bind ("<f5>" . recompile))

(use-package server
  :config
  (progn
    (unless (server-running-p)
      (server-start))))

(use-package shr
  :bind (("C-c w" . eww))
  :config
  (progn
    (setq shr-width 80)
    (setq shr-use-fonts nil)))

(use-package elec-pair
  :disabled
  :bind (("C-c t p" . electric-pair-mode))
  :config
  (progn
    (add-hook 'c-mode-hook 'electric-pair-mode)
    (add-hook 'python-mode-hook 'electric-pair-mode)))

(use-package cc-cmds
  :bind (("<M-up>" . move-text-up)
         ("<M-down>" . move-text-down))
  :preface
  (defvar c-mode-prettify-symbols-alist
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
  :config
  (progn
    (defun my-c-mode-hook ()
      (setq-local prettify-symbols-alist c-mode-prettify-symbols-alist)
      (prettify-symbols-mode)
      (add-to-list 'c-cleanup-list 'space-before-funcall))
    (add-hook 'c-mode-hook 'my-c-mode-hook)))

(use-package paren
  :bind (("C-c t m" . show-paren-mode)))

(use-package ffap
  :config (ffap-bindings))

(use-package winner
  :config
  (progn
    (winner-mode 1)
    (windmove-default-keybindings)))

(use-package pinentry
  :config (pinentry-start))

(use-package hideshow
  :diminish hs-minor-mode
  :config (add-hook 'prog-mode-hook 'hs-minor-mode))

(use-package python
  :config (setq python-shell-interpreter "python3"))

(use-package savehist-mode
  :config (savehist-mode t))

(use-package save-place-mode
  :config (save-place-mode t))

(use-package shell
  :bind(("C-c s s" . shell)
         ("C-c s e" . eshell))
  :config (add-hook 'shell-mode-hook 'guix-prettify-mode))

(use-package whitespace
  :bind (("C-c t w" . whitespace-mode))
  :config
  (progn
    (set-face-attribute 'whitespace-space
                        nil :background nil :foreground "dim gray")
    (mapcar (lambda (el)
              (delete el whitespace-style))
            '(newline newline-mark))))

(use-package calendar
  :commands calendar-current-date
  :config
  (progn
    (setq calendar-date-style 'european
          calendar-week-start-day 1)
    (defun mkdir-current-date ()
      (interactive)
      (mkdir (apply (lambda (m d y) (format "%s-%s-%s~" m d y))
                    (calendar-current-date))))))

(use-package time
  :commands display-time
  :config (setq display-time-24hr-format t))

(use-package info-look)

(use-package info
  :config (info-initialize))

(use-package doc-view)

(use-package time
  :commands display-time
  :config (setq display-time-24hr-format t))

(use-package info-look)

(use-package info
  :config (info-initialize))

(use-package doc-view)


;;;
;;; Structured editing
;;;

(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (add-hook 'prog-mode-hook 'smartparens-strict-mode)))


;;;
;;; Selection
;;;

(use-package expand-region
  :bind (("<f8>" . er/expand-region)))

(use-package multiple-cursors
  :bind (("<f7>" . mc/mark-next-like-this))
  :config (multiple-cursors-mode))

(use-package tex-mode
  :config
  (progn
    (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
    (defun compile-latex ()
      (interactive)
      (org-latex-export-to-latex)
      (recompile))))

(use-package imenu
  :bind (("C-c i" . imenu)))


;;;
;;; Tags
;;;

(use-package semantic
  :config
  (progn
    (global-semantic-decoration-mode t)
    (global-semantic-highlight-func-mode t)
    (global-semantic-show-unmatched-syntax-mode t)))

(use-package semantic/util-modes
  :after semantic
  :config
  (progn
    (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)))

(use-package gtags
  :config
  (progn
    (add-hook 'c-mode-hook '(lambda () (gtags-mode 1)))
    (add-hook 'lua-mode-hook '(lambda () (gtags-mode 1)))))

(use-package ggtags
  :after gtags
  :config
  (progn
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1))))
    (setq ggtags-mode-line-project-name nil)
    (setq ggtags-highlight-tag nil)))


;;;
;;; Completion
;;;

(use-package flx-ido
  :config
  (progn
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("<menu>" . smex)))

(use-package ido-vertical-mode
  :config (ido-vertical-mode 1))

(use-package helm
  :config (setq helm-locate-project-list my-projects))

(use-package company
  :diminish company-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'company-mode)
    (add-hook 'slime-repl-mode-hook 'company-mode)
    (setq company-clang-insert-arguments nil)
    (setq company-gtags-insert-arguments nil)
    (setq company-semantic-insert-arguments nil)))

(use-package company-quickhelp
  :after company
  :config
  (progn
    (eval-after-load 'company
      '(define-key company-active-map
         (kbd "C-c h") #'company-quickhelp-manual-begin))
    (company-quickhelp-mode t)))

(use-package company-lua
  :after company
  :config (add-to-list 'company-backends 'company-lua))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    (setq yas-snippet-dirs
          '("~/.emacs.d/snippets"
            "~/.guix-profile/share/emacs/yasnippet-snippets/"))
    (yas-reload-all)
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'latex-mode-hook 'yas-minor-mode)
    (add-hook 'message-mode-hook 'yas-minor-mode)))


;;;
;;; Email
;;;

(use-package gnus
  :bind (("C-c m g" . gnus))
  :preface
  (setq mail-user-agent 'gnus-user-agent)
  :config
  (progn
    (add-hook 'message-sent-hook 'gnus-score-followup-thread)

    (defun notmuch-to-gnus-group (file)
      "Calculate the Gnus group name from the given file name."
      (let ((group (file-name-directory (directory-file-name (file-name-directory file)))))
        (setq group (replace-regexp-in-string ".*/Maildir/" "nnimap+USER:" group))
        (setq group (replace-regexp-in-string "/$" "" group))
        (if (string-match ":$" group)
            (concat group "INBOX")
          (replace-regexp-in-string ":\\." ":" group))))

    (defun notmuch-goto-message-in-gnus ()
      "Open a summary buffer containing the current notmuch article."
      (interactive)
      (unless (gnus-alive-p) (with-temp-buffer (gnus)))
      (let ((group (notmuch-to-gnus-group (notmuch-show-get-filename)))
            (message-id
             (replace-regexp-in-string "\"" ""
                                       (replace-regexp-in-string "^id:" ""
                                                                 (notmuch-show-get-message-id)))))
        (if (and group message-id)
            (progn
              (gnus-summary-read-group group 1) ; have to show at least one old message
              (gnus-summary-refer-article message-id)) ; simpler than org-gnus method?
          (message "Couldn't get relevant infos for switching to Gnus."))))

    (defun send-buffer-as-mail ()
      (interactive)
      (let ((str (buffer-string)))
        (compose-mail)
        (save-excursion
          (message-goto-body)
          (insert str))))

    (defun switch-to-gnus (&optional arg)
      "Switch to a Gnus related buffer.
    Candidates are buffers starting with
     *mail or *reply or *wide reply
     *Summary or
     *Group*
    Use a prefix argument to start Gnus if no candidate exists."
      (interactive "P")
      (let (candidate
            (alist '(("^\\*\\(mail\\|\\(wide \\)?reply\\)" t)
                     ("^\\*Group")
                     ("^\\*Summary")
                     ("^\\*Article" nil (lambda ()
                                          (buffer-live-p
                                           gnus-article-current-summary))))))
        (catch 'none-found
          (dolist (item alist)
            (let (last
                  (regexp (nth 0 item))
                  (optional (nth 1 item))
                  (test (nth 2 item)))
              (dolist (buf (buffer-list))
                (when (and (string-match regexp (buffer-name buf))
                           (> (buffer-size buf) 0))
                  (setq last buf)))
              (cond ((and last (or (not test) (funcall test)))
                     (setq candidate last))
                    (optional
                     nil)
                    (t
                     (throw 'none-found t))))))
        (cond (candidate
               (switch-to-buffer candidate))
              (arg
               (gnus))
              (t
               (error "No candidate found")))))

    (mailcap-add-mailcap-entry "image"
                               "jpeg"
                               '((viewer  . "feh %s")
                                 (type    . "image/jpeg")))
    (mailcap-add-mailcap-entry "image"
                               "jpg"
                               '((viewer  . "feh %s")
                                 (type    . "image/jpg")))))

(use-package notmuch
  :preface (setq mail-user-agent 'gnus-user-agent)
  :commands notmuch-search
  :bind (("C-c m n" . notmuch)))


;;;
;;; Org
;;;

(use-package org
  :mode ("\\.notes\\'" . org-mode)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :config
  (progn
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . nil)
                                   (R . t)
                                   (python . t)
                                   (shell . t)))

    ;; (defun my-org-confirm-babel-evaluate (lang body)
    ;;   (not (string= lang "ditaa")))  ; don't ask for ditaa
    ;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

    (setq org-babel-python-command python-shell-interpreter)

    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale 1.5))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

    (setq org-capture-templates
          '(("c" "Note" item (file "~/.notes")
             "%?")

            ;; Requires org-capture-extension
            ;; https://github.com/sprig/org-capture-extension
            ("L" "Protocol Link" item (file "~/.web.org")
             "[[%:link][%:description]]")

            ("r" "Respond ro email" entry (file+headline "inbox.org" "Email")
             "[[mailto:%:fromaddress][%:fromname]]"
             :immediate-finish t
             :prepend t)

            ("f" "File email" entry (file+headline "inbox.org" "Email")
             "* %U %a by [[mailto:%:fromaddress][%:fromname]]\n\n%i%?\n"
             :immediate-finish nil
             :prepend nil)

            ("t" "Tasks" entry (file+headline ".notes" "Tasks")
             "* TODO %? \n%T" :prepend t)

            ("i" "TODO" entry (file "/TODO.gpg")
             "* %?")

            ("p" "pdfview" item (file "~/.pdf-notes")
             "%a %?")

            ("e" "emacs" plain (file "emacs.org")
             "%?")

            ("g" "guix" plain (file "guix.org")
             "%?")

            ("v" "video" plein (file "video.org")
             "%?")

            ("b" "blog" plain (file "~/public_html/IDEA.org")
             "%?")))

    ;; (org-link-set-parameters "file"
    ;;                   :face (lambda (path)
    ;;                           (when (not (file-remote-p path))
    ;;                             (if (file-exists-p path)
    ;;                                 'org-link 'org-warning))))

    ;; (defun my-org-mode-hook ()
    ;;   (dolist (face '(org-level-1 org-level-2 org-level-3
    ;;                               org-level-4 org-level-5))
    ;;     (set-face-attribute face nil
    ;;                         :weight 'normal
    ;;                         :height 1.0
    ;;                         :foreground "#000000")))

    ;; (add-hook 'org-mode-hook 'my-org-mode-hook)
    (add-to-list 'org-file-apps '("\\.png\\'" . "feh %s"))))

(use-package org-protocol)

(use-package org-protocol-capture-html)


;;;
;;; Version control
;;;

(use-package magit
  :bind (("C-c v s" . magit-status)
         ("C-c v p" . magit-dispatch-popup)
         ("C-c v l" . magit-list-repositories)
         ("C-c v v" . magit-stage))
  :config
  (progn
    (defun local-magit-initially-hide-unmerged (section)
      (and (not magit-insert-section--oldroot)
           (or (eq (magit-section-type section) 'unpushed)
               (equal (magit-section-value section) "@{upstream}..")
               (eq (magit-section-type section) 'stashes)
               (equal (magit-section-value section) "refs/stash"))
           'hide))

    (add-hook 'magit-section-set-visibility-hook
              'local-magit-initially-hide-unmerged)

    (defun me-update-magit-repository-directories ()
      (setq magit-repository-directories my-projects))

    (me-update-magit-repository-directories)

    (add-hook 'magit-repolist-mode-hook
              'me-update-magit-repository-directories)))

(use-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode +1))


;;;
;;; Lisp
;;;

(use-package elisp-mode
  :config (add-hook 'emacs-lisp-mode-hook 'show-paren-mode))

(use-package lisp-mode
  :config (add-hook 'after-save-hook 'check-parens nil t))

(use-package scheme
  :preface
  (setq scheme-prettify-symbols-alist
        '(("lambda" . ?λ)
          ("lambda*" . (?λ (Br . Bl) ?*))
          ("#t" . ?T)
          ("#f" . ?F)
          ("not" . ?¬)
          ("and" . ?∧)
          ("or" . ?∨)
          ("eq?" . ≡)
          ("<=" . ?≤)
          (">=" . ?≥)))
  :config
  (progn
    (defun my-scheme-mode-hook ()
      (setq-local prettify-symbols-alist scheme-prettify-symbols-alist)
      (prettify-symbols-mode)
      (show-paren-mode))
    (add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
    (add-hook 'geiser-repl-mode-hook 'my-scheme-mode-hook)
    (setq geiser-active-implementations (quote (guile)))))


;;;
;;; Guix
;;;

(use-package guix
  :load-path "/home/natsu/src/emacs-guix/elisp"
  :config
  (progn
    (use-package guix-devel
      :diminish guix-devel-mode
      :config (add-hook 'scheme-mode-hook 'guix-devel-mode))

    (use-package guix-build-log
      :diminish guix-build-log-minor-mode)

    (use-package guix-external
      :config
      (progn
        (setq guix-guile-program '("/home/natsu/src/guix/pre-inst-env"
                                   "guile" "--no-auto-compile"))))

    (use-package guix-repl
      :config
      (setq guix-directory "~/src/guix"))

    (use-package guix-location
      :bind (("C-c g e" . guix-edit)))

    (use-package guix-read
      :config (setq guix-read-package-name-function
                    'guix-read-package-name-at-point))

    (use-package guix-utils
      :after org
      :config
      (setq guix-find-file-function 'org-open-file))

    (use-package guix-command
      :bind (("C-c g p" . guix)))

    (use-package guix-help
      :bind (("C-c g b" . guix-switch-to-buffer)
             ("C-c g x" . guix-extended-command))
      :config
      (progn
        (defun guix-src-grep (regexp)
          (interactive "sGREP: ")
          (rgrep regexp
                 "*.scm"
                 (concat guix-directory "/gnu/packages")))
        (setq guix-directory "~/src/guix")
        (add-hook 'proced-post-display-hook 'guix-prettify-mode)
        (add-hook 'dired-mode-hook 'guix-prettify-mode)))))


;;;
;;; Geiser
;;;

(use-package geiser
  :config
  (progn
    (use-package geiser-guile
      :commands geiser-repl-mode
      :config
      (with-eval-after-load 'geiser-guile
        (add-to-list 'geiser-guile-load-path "/home/natsu/src/guix")
        (setq geiser-guile-binary '("guile" "--no-auto-compile"))))

    (use-package geiser-doc)))


(use-package slime
  :disabled
  :init
  (progn
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "/home/natsu/.guix-profile/bin/sbcl")
    (load-file "/home/natsu/.stumpwm.d/modules/util/swm-emacs/stumpwm-utils.el")
    (load-file "/home/natsu/.stumpwm.d/modules/util/swm-emacs/stumpwm-mode.el")))

(use-package slime-company
  :after company
  :config
  (progn
    (slime-setup '(slime-company))))


;;;
;;; Disabled
;;;

(use-package page-break-lines
  :disabled
  :diminish page-break-lines-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'page-break-lines-mode)
    (add-hook 'scheme-mode-hook 'page-break-lines-mode)))

(use-package edit-server
  :init
  (progn
    (edit-server-start)))

(use-package paredit
  :disabled
  :diminish paredit-mode
  :config
  (progn
    (add-hook 'scheme-mode-hook 'paredit-mode)
    (add-hook 'minibuffer-inactive-mode-hook 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)))

(use-package smart-mode-line
  :disabled
  :config
  (progn
    (sml/setup)))

(use-package hydra
  :disabled
  :config
  (progn
    (defhydra hydra-buffer (global-map "<f7>")
      "buffer"
      ("n" next-buffer "next")
      ("p" previous-buffer "previous"))))



;;;
;;; Misc
;;;

(use-package avy
  :config (avy-setup-default))

(use-package aggressive-indent
  :bind (("C-c t a" . aggressive-indent-mode))
  :init
  (mapcar (lambda (mode)
            (add-hook mode 'aggressive-indent-mode))
          '(scheme-mode-hook emacs-lisp-mode-hook lisp-mode-hook c-mode-hook)))

(use-package browse-url
  :commands browse-url-mpv
  :config
  (progn
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
            ("^https?://w*\\.?youtube.com/watch\\?v=.*" . browse-url-mpv)
            ("^https?://w*\\.?github.com/.*" . browse-url-chromium)
            ("." . browse-url-conkeror)))

    (defun browse-url-mpv-remote (url &optional new-window)
      "Ask the mpv video player to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-mpv-arguments' to mpv."
      (interactive (browse-url-interactive-arg "URL: "))
      (setq url (browse-url-encode-url url))
      (let* ((process-environment (browse-url-process-environment)))
        (apply 'start-process
               (concat "mpv " url) nil
               browse-url-mpv-remote-program
               (append
                browse-url-mpv-remote-arguments
                (list (car (split-string url "&")))))))))

(use-package debbugs-browse
  :after browse-url
  :config
  (progn
    (add-to-list 'browse-url-browser-function
                 '("^https?://debbugs\\.gnu\\.org/.*" . debbugs-browse-url))
    (defun debbugs-gnu-guix ()
      (interactive)
      (debbugs-gnu '("serious" "important" "normal") '("guix")))
    (defun debbugs-gnu-guix-patches ()
      (interactive)
      (debbugs-gnu '("serious" "important" "normal") '("guix-patches")))))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions
        (quote
         (("docx" . "libreoffice")
          ("doc" . "libreoffice")
          ("xlsx" . "libreoffice")
          ("xls" . "libreoffice")
          ("mp3" . "mpv")
          ("webm" . "mpv")
          ("mkv" . "mpv")
          ("mp4" . "mpv")
          ("flv" . "mpv")
          ("png" . "feh")
          ("jpg" . "feh")
          ("jpeg" . "feh")))))

(use-package debbugs-gnu
  :commands debbugs-gnu
  :config (add-to-list 'debbugs-gnu-all-packages "guix-patches"))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind (("C-c u" . undo-tree-visualize))
  :config (add-hook 'prog-mode-hook 'undo-tree-mode))

(use-package projectile-global-mode
  :bind (("C-c p m" . projectile-commander))
  :init
  (progn
    (setq projectile-mode-line nil)
    (projectile-global-mode))
  :config
  (progn
    (setq projectile-completion-system (quote ido))
    (setq projectile-use-git-grep t)))

(use-package web-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (setq-default web-mode-markup-indent-offset 2)
    (setq-default web-mode-code-indent-offset 2)
    (setq-default web-mode-css-indent-offset 2)
    (setq-default web-mode-style-padding 2)
    (add-hook 'web-mode-hook 'tern-mode)))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (progn
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)
    (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
    (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
    (setq TeX-save-query nil)))

(use-package google-translate-mode
  :bind (("C-c t t" . google-translate-mode))
  :config (add-hook 'Info-mode-hook 'google-translate-mode))

(use-package imaxima
  :commands imaxima)

(use-package engine-mode
  :config
  (progn
    (engine-mode t)
    (setq engine/browser-function 'browse-url-chromium)
    (defengine searx
      "http://searx.tk/?q=%s"
      :keybinding "x")
    (defengine youtube
      "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
      :keybinding "y")
    (defun searx-email ()
      (interactive)
      (require 's)
      (engine/search-searx
       (s-chop-prefix "<" (s-chop-suffix ">" (thing-at-point 'email)))))))

(use-package flyspell
  :config (add-hook 'message-mode-hook 'flyspell-mode))

(use-package flycheck
  :config
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (setq flycheck-display-errors-delay 2)))

(use-package sr-speedbar
  :bind (("C-c s b" . sr-speedbar-toggle)))

(use-package prog-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'prettify-symbols-mode)
    (setq prettify-symbols-unprettify-at-point 'right-edge)))

(use-package haskell-mode
  :preface
  (defvar haskell-prettify-symbols-alist
    '(("::"     . ?∷)
      ("forall" . ?∀)
      ("exists" . ?∃)
      ("->"     . ?→)
      ("<-"     . ?←)
      ("=>"     . ?⇒)
      ("~>"     . ?⇝)
      ("<~"     . ?⇜)
      ("."      . ?∘)
      ("<>"     . ?⨂)
      ("msum"   . ?⨁)
      ("\\"     . ?λ)
      ("not"    . ?¬)
      ("&&"     . ?∧)
      ("||"     . ?∨)
      ("/="     . ?≠)
      ("<="     . ?≤)
      (">="     . ?≥)
      ("<<<"    . ?⋘)
      (">>>"    . ?⋙)

      ("`elem`"             . ?∈)
      ("`notElem`"          . ?∉)
      ("`member`"           . ?∈)
      ("`notMember`"        . ?∉)
      ("`union`"            . ?∪)
      ("`intersection`"     . ?∩)
      ("`isSubsetOf`"       . ?⊆)
      ("`isProperSubsetOf`" . ?⊂)
      ("undefined"          . ?⊥)

      ("True"  . ?T)
      ("False" . ?F)

      (".." . ?…)))
  :config
  (progn
    (defun my-haskell-mode-hook ()
      (setq-local prettify-symbols-alist haskell-prettify-symbols-alist)
      (prettify-symbols-mode)
      (show-paren-mode))
    (add-hook 'haskell-mode-hook 'my-scheme-mode-hook)
    (add-hook 'inferior-haskell-mode-hook 'my-haskell-mode-hook)))

(use-package rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode)))

(use-package tern
  :config
  (add-hook 'rjsx-mode-hook 'tern-mode))

(use-package company-tern
  :after company
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package emms
  :config
  (progn
    (require 'emms-playlist-mode)
    (setq emms-source-file-default-directory "/srv/archive/cool-music/")))

(use-package emms-player-mpv
  :config
  (progn
    (add-to-list 'emms-player-list 'emms-player-mpv)
    (mapcar (lambda (parameter)
              (add-to-list 'emms-player-mpv-parameters parameter))
            '("--no-resume-playback"
              "--keep-open=no"))))

(use-package wordgen
  :config
  (progn
    (defun random-words ()
      (interactive)
      (mapcar (lambda (word) (insert (concat word " ")))
              (wordgen
               '((result (concat-reeval [(2 1) (5 2) (4 3)] syl))
                 (syl (++ c v coda))
                 (c [(4 "p") (5 "t") (5 "k") (3 "m")
                     (4 "n") (3 "s") (4 "l") (3 "r")])
                 (v ["a" "e" "i" "o" "u"])
                 (coda [(4 "") "m" "n"]))
               :word-count 5)))

    (defun random-words-javascript ()
      (interactive)
      (insert "[")
      (mapcar (lambda (word) (insert (concat "'" word "'" ", ")))
              (wordgen
               '((result (concat-reeval [(2 1) (5 2) (4 3)] syl))
                 (syl (++ c v coda))
                 (c [(4 "p") (5 "t") (5 "k") (3 "m")
                     (4 "n") (3 "s") (4 "l") (3 "r")])
                 (v ["a" "e" "i" "o" "u"])
                 (coda [(4 "") "m" "n"]))
               :word-count 5))
      (insert "]"))))

(use-package indium-repl
  :preface
  (require 'indium)
  :config
  (progn
    (defvar indium-repl-prettify-symbols-alist
      '(("function" . 955)
        ("=>" . ?⇒)))
    (defun my-indium-repl-mode-hook ()
      (setq-local prettify-symbols-alist indium-repl-prettify-symbols-alist)
      (prettify-symbols-mode)
      (show-paren-mode))
    (add-hook 'js2-mode-hook 'my-indium-repl-mode-hook)
    (add-hook 'indium-repl-mode-hook 'my-indium-repl-mode-hook)))

(use-package markdown-mode
  :config (setq markdown-fontify-code-blocks-natively t))

(use-package bash-completion
  :config
  (progn
    (autoload 'bash-completion-dynamic-complete
      "bash-completion" "BASH completion hook")
    (add-hook 'shell-dynamic-complete-functions
              'bash-completion-dynamic-complete)))

(use-package writeroom-mode
  :config
  (progn
    (setq writeroom-bottom-divider-width 0)
    (defun manoj-dark-more ()
      "Make fringes and header dark."
      (custom-theme-set-faces
       'manoj-dark
       '(fringe ((t (:background "black" :foreground "Wheat"))))
       '(header-line
         ((t (:background "black" :foreground "grey90" :height 0.9))))))
    (defun manoj-dark-default ()
      "Make fringes and header default color."
      (custom-theme-set-faces
       'manoj-dark
       '(fringe ((t (:background "grey30" :foreground "Wheat"))))
       '(header-line
         ((t (:box (:line-width -1 :color "grey20" :style released-button)
                   :background "grey20"
                   :foreground "grey90"
                   :height 0.9))))))
    (add-hook 'writeroom-mode-hook (lambda () (if writeroom-mode
                                             (manoj-dark-more)
                                           (manoj-dark-default))))))

(use-package git-auto-commit-mode
  :config
  (progn
    ;; (setq gac-automatically-push-p t)

    ;; Without a .dir-locals.el file
    ;; https://www.emacswiki.org/emacs/DirectoryVariables
    (dir-locals-set-class-variables 'dir-local-var-git-auto-commit
				    (quote
				     ((nil . ((mode . git-auto-commit))))))

    (dir-locals-set-directory-class org-directory
                                    'dir-local-var-git-auto-commit)))

(use-package sh-script
  :mode ("PKGBUILD\\'" . shell-script-mode))

(use-package dashboard
  :config (dashboard-setup-startup-hook))

(use-package eval-in-repl
  :config
  (progn
    (add-hook 'geiser-mode-hook
              '(lambda () (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser)))
    (add-hook 'sh-mode-hook
              '(lambda() (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))))

(use-package beginend
  :diminish
  beginend-bs-mode
  beginend-compilation-mode
  beginend-dired-mode
  beginend-elfeed-search-mode
  beginend-global-mode
  beginend-ibuffer-mode
  beginend-magit-status-mode
  beginend-message-mode
  beginend-notmuch-search-mode
  beginend-occur-mode
  beginend-org-agenda-mode
  beginend-prodigy-mode
  beginend-prog-mode
  beginend-prog-mode
  beginend-recentf-dialog-mode
  beginend-vc-dir-mode

  :config (beginend-global-mode))

(provide 'wigust-user-init-file)
;;; .emacs ends here
