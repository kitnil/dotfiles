;;; .emacs --- user-init-file

;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

;;; Commentary:
;;
;; GNU Emacs configuration file.
;;
;; Inspired by https://github.com/jwiegley/dot-emacs

;;; Code:

(setq user-mail-address "go.wigust@gmail.com")
(setq user-full-name    "Oleg Pykhalov")

(setq initial-scratch-message "")
(setq initial-major-mode 'fundamental-mode)

(when t (setq use-package-verbose t) (use-package benchmark-init))

(setq display-time-24hr-format t)
(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)

(setq default-input-method "russian-computer")

(setq load-prefer-newer t)

(package-initialize)

(setq inhibit-compacting-font-caches t)

(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))
(setq tab-always-indent 'complete)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell t)

(setq save-place-mode t)
(setq mouse-yank-at-point t)

(setq vc-follow-symlinks t)

(setq prettify-symbols-unprettify-at-point 'right-edge)

(defconst projects-directory "/srv/git")

(defun toggle-manoj-dark-theme ()
  "Toggle theme for night nerding."
  (interactive)
  (load-theme 'manoj-dark)
  (custom-theme-set-variables
   'manoj-dark '(company-quickhelp-color-background "black"))
  (toggle-whitespace-color nil))

(defun list-projects (directory)
  "Return a list of projects in the DIRECTORY."
  (interactive)
  (directory-files (expand-file-name directory)
                   t
                   "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))

(defun copy-current-buffer-file-name ()
  (interactive)
  (let ((name (buffer-file-name))) (kill-new name) (message name)))

(defun find-user-init-file ()
  "Find user init file."
  (interactive)
  (find-file user-init-file))

(bind-key "C-c 0"         #'copy-current-buffer-name)
(bind-key "C-c e c"       #'find-user-init-file)
(bind-key "<Scroll_Lock>" #'scroll-lock-mode)
(bind-key "C-c t l"       #'toggle-truncate-lines)
(bind-key "<C-mouse-4>"   #'text-scale-increase)
(bind-key "<C-mouse-5>"   #'text-scale-decrease)
(bind-key "C-c b i"       #'ibuffer)
(bind-key "C-c b s"       #'scratch)
(bind-key "<f5>"          #'compile)
(bind-key "C-c t p"       #'electric-pair-mode)
(bind-key "C-c m g"       #'gnus)
(bind-key "C-c m s g"     #'switch-to-gnus)
(bind-key "C-c t m"       #'show-paren-mode)
(bind-key "C-c i"         #'imenu)
(bind-key "C-c f"         #'ffap)

(setq mail-user-agent 'gnus-user-agent)
(setq send-mail-function #'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.gmail.com")

(defun send-buffer-as-mail ()
  (interactive)
  (let ((str (buffer-string)))
    (compose-mail)
    (save-excursion
      (message-goto-body)
      (insert str))))

(put 'narrow-to-region 'disabled nil)

(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))

(use-package rainbow-mode :bind (("C-c t r" . rainbow-mode)))

(defun turn-on-truncate-lines ()
  "Convenience method to turn on `truncate-lines'."
  (interactive)
  (toggle-truncate-lines 1))

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(hook-into-modes #'turn-on-truncate-lines
                 '(dired-mode-hook occur-mode-hook diff-mode-hook))

(add-hook 'message-mode-hook #'flyspell-mode)

(eval-after-load 'dired (progn (turn-on-gnus-dired-mode)
                               (dired-async-mode t)))

(with-eval-after-load 'dired-x
  (mapcar (lambda (extension)
            (add-to-list 'dired-guess-shell-alist-user
                         `(,extension
                           ,"mpv --no-resume-playback --keep-open=no")))
          '("\\.mp4$" "\\.webm$")))

(defun scratch ()
  "Open scratch buffer."
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (lisp-interaction-mode)))

(use-package server :config (unless (server-running-p) (server-start)))

(use-package shr
  :commands eww
  :bind (("C-c w" . eww))
  :config (setq shr-width 80) (setq shr-use-fonts nil))

(use-package move-text
  :bind (("<M-up>" . move-text-up)
         ("<M-down>" . move-text-down)))

(use-package cc-cmds
  :mode (("\\.[ch]\\'" . c-mode))
  :preface
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
  :config
  (set (make-local-variable 'prettify-symbols-alist)
       c--prettify-symbols-alist)
  (mapc (lambda (mode) (add-hook 'c-mode-hook mode))
        '(show-paren-mode prettify-symbols-mode))
  (add-to-list 'c-cleanup-list 'space-before-funcall))

(use-package winner
  :defer 5
  :config
  (winner-mode 1)
  (windmove-default-keybindings))

(use-package hideshow
  :diminish hs-minor-mode
  :config (add-hook 'prog-mode-hook 'hs-minor-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config (setq python-shell-interpreter "python3"))

(use-package saveplace
  :config (save-place-mode t))

(use-package savehist
  :defer 5
  :config (savehist-mode t))

(use-package shell
  :bind (("C-c s s" . shell)
         ("C-c s e" . eshell))
  :config (add-hook 'shell-mode-hook #'guix-prettify-mode))

(use-package whitespace
  :config
  (defun toggle-whitespace-color (light)
    (interactive)
    (let ((foreground (if light "gainsboro" "gray15")))
      (mapc (lambda (font)
              (set-face-attribute font nil
                                  :background nil :foreground foreground))
            '(whitespace-space whitespace-indentation)))
    (set-face-attribute whitespace-tab nil
                        :background nil :foreground "gold4"))
  (toggle-whitespace-color t)
  (mapc (lambda (el) (delete el whitespace-style)) '(newline newline-mark))
  (add-hook 'prog-mode-hook #'whitespace-mode))

(use-package calendar
  :defer 5
  :config
  (defun mkdir-current-date ()
    "Make directrory with current date."
    (interactive)
    (mkdir (apply (lambda (m d y) (format "%s-%s-%s~" m d y))
                  (calendar-current-date)))))

(use-package time
  :commands display-time-world
  :bind (("C-c a a" . display-time-world))
  :config
  (setq display-time-world-time-format "%Z\t%d %B %H:%M")
  (setq display-time-world-list '(("Europe/Moscow"    "Europe/Moscow")
                                  ("Europe/Berlin"    "Europe/Berlin")
                                  ("Europe/London"    "Europe/London")
                                  ("Europe/Istanbul"  "Europe/Istanbul")
                                  ("America/Winnipeg" "America/Winnipeg")
                                  ("America/New_York" "America/New_York")
                                  ("Asia/Tokyo"       "Asia/Tokyo"))))

(use-package info
  :defer 5
  :config
  (info-initialize)
  (add-to-list 'Info-directory-list "/home/natsu/src/guile-chickadee/doc"))

(use-package info-look :defer 5)

(use-package doc-view :defer 5)

(use-package smartparens
  :defer 5
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'sp)
  (sp-use-smartparens-bindings)
  (smartparens-global-strict-mode))

(use-package expand-region :bind (("<f8>" . er/expand-region)))

(use-package multiple-cursors
  :bind (("<f7>" . mc/mark-next-like-this))
  :config (multiple-cursors-mode))

(use-package tex-mode
  :mode (("\\.tex\\'" . latex-mode)
         ("\\.texi\\'" . texinfo-mode))
  :config (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))

(use-package semantic
  :defer t
  :config
  (global-semantic-decoration-mode t)
  (global-semantic-highlight-func-mode t)
  (global-semantic-show-unmatched-syntax-mode t))

(use-package semantic/util-modes
  :after semantic
  :config
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode))

(use-package lua-mode :mode (("\\.lua\\'" . lua-mode)))

(use-package gtags
  :after (cc-cmds lua-mode)
  :config
  (hook-into-modes (lambda () (gtags-mode 1)) '(c-mode-hook lua-mode-hook)))

(use-package ggtags
  :after gtags
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  (setq ggtags-mode-line-project-name nil)
  (setq ggtags-highlight-tag nil))

(use-package ido
  ;; https://masteringemacs.org/article/introduction-to-ido-mode
  :defer 5
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (ido-mode 'buffers))

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode 1))

(use-package smex
  :after ido
  :bind (("M-x"    . smex)
         ("M-X"    . smex-major-mode-commands)
         ("<menu>" . smex)))

(use-package ido-vertical-mode :after ido :config (ido-vertical-mode 1))

(use-package helm
  :defer 5
  :bind (("C-c h i" . helm-imenu)
         ("C-c h o" . helm-occur))
  :config
  (setq helm-locate-project-list (list-projects projects-directory)))

(use-package company
  :defer 5
  :config
  (setq company-clang-insert-arguments nil)
  (setq company-gtags-insert-arguments nil)
  (setq company-semantic-insert-arguments nil)
  (global-company-mode))

(use-package company-quickhelp
  :after company
  :config
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  (company-quickhelp-mode t))

(use-package company-lua
  :after company
  :config (add-to-list 'company-backends 'company-lua))

(use-package yasnippet
  :defer 5
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           "~/.guix-profile/share/emacs/yasnippet-snippets/"))
  (yas-reload-all)
  (yas-global-mode))

(use-package gitpatch
  :commands gitpatch-mail
  :config
  (setq gitpatch-mail-database (list "guix-patches@gnu.org")))

(use-package gnus
  :bind (("C-c m g g" . gnus)
         ("C-c m g s" . switch-to-gnus))
  :config
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
             (error "No candidate found"))))))

(use-package notmuch
  :commands notmuch-search
  :bind (("C-c m n" . notmuch))
  :config
  (setq nnir-notmuch-remove-prefix "/home/natsu/Maildir/")

  (defun notmuch-to-gnus-group (file)
    "Calculate the Gnus group name from the given file name."
    (let ((group (file-name-directory
                  (directory-file-name (file-name-directory file)))))
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
           (replace-regexp-in-string
            "\"" ""
            (replace-regexp-in-string "^id:" ""
                                      (notmuch-show-get-message-id)))))
      (if (and group message-id)
          (progn
            (gnus-summary-read-group group 1) ; show at least one old message
            (gnus-summary-refer-article message-id))
        (message "Couldn't get relevant infos for switching to Gnus.")))))


;;;
;;; Org
;;;

(use-package org
  :mode ("\\.notes\\'" . org-mode)
  :bind (("C-c c c" . org-capture)
         ("C-c c a" . org-agenda)
         ("C-c c l" . org-store-link))
  :config
  (defun org-compile-latex ()
    (interactive)
    (org-latex-export-to-latex)
    (recompile))

  (setq org-agenda-files nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . nil)
                                 (R . t)
                                 (python . t)
                                 (shell . t)))

  ;; (defun my-org-confirm-babel-evaluate (lang body)
  ;;   (not (string= lang "ditaa")))  ; don't ask for ditaa
  ;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

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

          ("m" "music" plain (file "music.org")
           "%?")

          ("v" "video" plain (file "video.org")
           "%?")

          ("b" "blog" plain (file "~/public_html/IDEA.org")
           "%?")))
  (add-to-list 'org-file-apps '("\\.png\\'" . "feh %s")))

(use-package magit
  :bind (("C-c v s" . magit-status)
         ("C-c v p" . magit-dispatch-popup)
         ("C-c v l" . magit-list-repositories)
         ("C-c v v" . magit-stage)
         ("C-c v r" . magit-diff-toggle-refine-hunk))
  :init
  (defun update-magit-repository-directories ()
    (setq magit-repository-directories
          (list-projects projects-directory)))
  (add-hook 'after-init-hook 'update-magit-repository-directories)
  :config
  (setq magit-log-arguments (list "--graph" "--color" "--decorate" "-n64"))
  (setq magit-log-section-arguments (list "-n256" "--decorate"))
  (defun local-magit-initially-hide-unmerged (section)
    (and (not magit-insert-section--oldroot)
         (or (eq (magit-section-type section) 'unpushed)
             (equal (magit-section-value section) "@{upstream}..")
             (eq (magit-section-type section) 'stashes)
             (equal (magit-section-value section) "refs/stash"))
         'hide))

  (add-hook 'magit-section-set-visibility-hook
            'local-magit-initially-hide-unmerged))

(use-package git-gutter
  :defer 5
  :diminish git-gutter-mode
  :config (global-git-gutter-mode 1))

(use-package browse-at-remote :bind (("C-c v o" . browse-at-remote)))

(use-package elisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("\\.emacs\\'" . emacs-lisp-mode))
  :preface
  (defconst elisp--prettify-symbols-alist
    '(("lambda"  . ?λ)
      ("lambda*" . (?λ (Br . Bl) ?*))
      ("not"     . ?¬)
      ("and"     . ?∧)
      ("or"      . ?∨)
      ("eq?"     . ≡)
      ("<="      . ?≤)
      (">="      . ?≥)
      ("->"      . ?→)))
  :config
  (setq-local prettify-symbols-alist elisp--prettify-symbols-alist)
  (mapc (lambda (mode) (add-hook 'emacs-lisp-mode-hook mode))
        '(show-paren-mode prettify-symbols-mode)))

(use-package lisp-mode
  :defer t
  :config (add-hook 'after-save-hook 'check-parens nil t))

(use-package scheme
  :mode (("\\.tmpl\\'" . scheme-mode)
         ("\\.sxml\\'" . scheme-mode)
         ("\\.scm\\'" . scheme-mode))
  :preface
  (defconst scheme--prettify-symbols-alist
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
  :config
  (setq geiser-active-implementations (quote (guile)))
  (setq-local prettify-symbols-alist scheme--prettify-symbols-alist)
  (mapc (lambda (mode) (add-hook 'scheme-mode-hook mode))
        '(show-paren-mode
          prettify-symbols-mode)))


;;;
;;; Guix
;;;

(use-package guix
  :mode (("\\.tmpl\\'" . scheme-mode)
         ("\\.sxml\\'" . scheme-mode)
         ("\\.scm\\'" . scheme-mode))
  :bind (("C-c g e" . guix-edit)
         ("C-c g p" . guix)
         ("C-c g b" . guix-switch-to-buffer)
         ("C-c g x" . guix-extended-command))
  :init
  (add-hook 'scheme-mode-hook 'guix-devel-mode)
  (add-hook 'proced-post-display-hook 'guix-prettify-mode)
  (add-hook 'dired-mode-hook 'guix-prettify-mode)
  :config
  (add-to-list 'Info-directory-list "/home/natsu/src/guix/doc")
  (setq guix-directory "~/src/guix")
  (setq guix-guile-program '("/home/natsu/src/guix/pre-inst-env"
                             "guile" "--no-auto-compile"))
  (setq guix-read-package-name-function #'guix-read-package-name-at-point)

  (use-package guix-utils
    :after org
    :config
    (setq guix-find-file-function 'org-open-file))

  (use-package guix-derivation))


;;;
;;; Geiser
;;;

(use-package geiser-guile
  :after scheme
  :config
  (add-hook 'scheme-mode-hook #'geiser-mode)
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "/home/natsu/src/guix")
    (setq geiser-guile-binary '("guile" "--no-auto-compile"))))

(use-package geiser-doc :after geiser-guile)


;;;
;;; Slime
;;;

(use-package slime
  :defer t
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/home/natsu/.guix-profile/bin/sbcl")

  (require 'cl)

  (defun slime-style-init-command (port-filename _coding-system extra-args)
    "Return a string to initialize Lisp."
    (let ((loader (if (file-name-absolute-p slime-backend)
                      slime-backend
                    (concat slime-path slime-backend))))
      ;; Return a single form to avoid problems with buffered input.
      (format "%S\n\n"
              `(progn
                 (load ,(slime-to-lisp-filename (expand-file-name loader))
                       :verbose t)
                 (funcall (read-from-string "swank-loader:init"))
                 (funcall (read-from-string "swank:start-server")
                          ,(slime-to-lisp-filename port-filename)
                          ,@extra-args)))))

  (defun slime-style (&optional style)
    (interactive
     (list (intern-soft (read-from-minibuffer "Style: " "nil"))))
    (lexical-let ((style style))
      (slime-start
       :init (lambda (x y)
               (slime-style-init-command
                x y `(:style ,style :dont-close t)))))))

(use-package slime-company
  :after company
  :config
  (slime-setup '(slime-company)))


;;;
;;; Misc
;;;

(use-package avy
  :defer 5
  :config (avy-setup-default))

(use-package aggressive-indent
  :defer 5
  :bind (("C-c t a" . aggressive-indent-mode))
  :init
  (mapc (lambda (mode) (add-hook mode #'aggressive-indent-mode))
        '(scheme-mode-hook emacs-lisp-mode-hook lisp-mode-hook c-mode-hook)))

(use-package browse-url
  :defer 5
  :config
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
  (add-to-list 'browse-url-browser-function
               '("^https?://debbugs\\.gnu\\.org/.*" . debbugs-browse-url))

  (setq browse-url-browser-function
        `(("^ftp://.*" . browse-ftp-tramp)
          ("^https?://w*\\.?youtube.com/watch\\?v=.*" . browse-url-mpv)
          ("^https?://w*\\.?youtube.com/.*" . browse-url-chromium)
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
              (list (car (split-string url "&"))))))))

(use-package debbugs
  :defer 5
  :config
  (use-package debbugs-browse
    :after browse-url
    :config
    (setq debbugs-gnu-default-packages (list "guix" "guix-patches"))
    (defun debbugs-gnu-guix ()
      (interactive)
      (debbugs-gnu '("serious" "important" "normal") '("guix")))
    (defun debbugs-gnu-guix-patches ()
      (interactive)
      (debbugs-gnu '("serious" "important" "normal") '("guix-patches")))))

(use-package which-key
  :defer 5
  :diminish which-key-mode
  :config (which-key-mode))

(use-package dired-open
  :defer 5
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
  (setq projectile-mode-line nil)
  (projectile-global-mode)
  :config
  (setq projectile-completion-system (quote ido))
  (setq projectile-use-git-grep t))

(use-package web-mode
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ("\\.php?\\'"      . web-mode)
         ("\\.html?\\'"     . web-mode))
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-style-padding 2)
  (add-hook 'web-mode-hook 'tern-mode))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-save-query nil))

(use-package google-translate-mode
  :bind   (("C-c t t" . google-translate-mode))
  :config (setq trans-target "ru"))

(use-package imaxima
  :commands imaxima
  :config   (setq imaxima-scale-factor 1.5))

(use-package engine-mode
  :config
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
     (s-chop-prefix "<" (s-chop-suffix ">" (thing-at-point 'email))))))

(use-package flycheck
  :defer 5
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq flycheck-display-errors-delay 2))

(use-package sr-speedbar
  :bind (("C-c s b" . sr-speedbar-toggle)))

(use-package haskell-mode
  :defer t
  :preface
  (defconst haskell--prettify-symbols-alist
    '(("::"                 . ?∷)
      ("forall"             . ?∀)
      ("exists"             . ?∃)
      ("->"                 . ?→)
      ("<-"                 . ?←)
      ("=>"                 . ?⇒)
      ("~>"                 . ?⇝)
      ("<~"                 . ?⇜)
      ("."                  . ?∘)
      ("<>"                 . ?⨂)
      ("msum"               . ?⨁)
      ("\\"                 . ?λ)
      ("not"                . ?¬)
      ("&&"                 . ?∧)
      ("||"                 . ?∨)
      ("/="                 . ?≠)
      ("<="                 . ?≤)
      (">="                 . ?≥)
      ("<<<"                . ?⋘)
      (">>>"                . ?⋙)
      ("`elem`"             . ?∈)
      ("`notElem`"          . ?∉)
      ("`member`"           . ?∈)
      ("`notMember`"        . ?∉)
      ("`union`"            . ?∪)
      ("`intersection`"     . ?∩)
      ("`isSubsetOf`"       . ?⊆)
      ("`isProperSubsetOf`" . ?⊂)
      ("undefined"          . ?⊥)
      ("True"               . ?T)
      ("False"              . ?F)
      (".."                 . ?…)))
  :config
  (set (make-local-variable 'prettify-symbols-alist)
       haskell--prettify-symbols-alist)
  (mapc (lambda (mode) (add-hook 'inferior-haskell-mode-hook mode))
        '(show-paren-mode
          prettify-symbols-mode)))

(use-package rjsx-mode :mode (("\\.js\\'" . rjsx-mode)))

(use-package tern
  :after rjsx-mode
  :config (add-hook 'rjsx-mode-hook 'tern-mode))

(use-package company-tern
  :after company
  :config (add-to-list 'company-backends 'company-tern))

(use-package emms
  :bind (("C-c m m m" . emms)
         ("C-c m m n" . emms-next)
         ("C-c m m p" . emms-previous)
         ("C-c m m d" . emms-play-directory)
         ("C-c m m s" . emms-stop))
  :config
  (require 'emms-playlist-mode)
  (setq emms-source-file-default-directory "~/Music"))

(use-package emms-player-mpv
  :after emms
  :config
  (add-to-list 'emms-player-list 'emms-player-mpv)
  (mapcar (lambda (parameter)
            (add-to-list 'emms-player-mpv-parameters parameter))
          '("--no-resume-playback"
            "--keep-open=no")))

(use-package wordgen
  :defer 5
  :config
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
    (insert "]")))

(use-package indium-repl
  :defer t
  :config
  (defvar indium-repl-prettify-symbols-alist
    '(("function" . 955)
      ("=>" . ?⇒)))
  (defun my-indium-repl-mode-hook ()
    (setq-local prettify-symbols-alist indium-repl-prettify-symbols-alist)
    (prettify-symbols-mode)
    (show-paren-mode))
  (add-hook 'js2-mode-hook 'my-indium-repl-mode-hook)
  (add-hook 'indium-repl-mode-hook 'my-indium-repl-mode-hook))

(use-package markdown-mode
  :defer t
  :config (setq markdown-fontify-code-blocks-natively t))

(use-package bash-completion
  :after shell
  :config
  (autoload 'bash-completion-dynamic-complete
    "bash-completion" "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))

(use-package writeroom-mode
  :commands writeroom-mode
  :config
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
                                         (manoj-dark-default)))))

(use-package eval-in-repl
  :defer 5
  :config
  (defun toggle-eir-jump-after-eval ()
    (interactive)
    (if eir-jump-after-eval
        (setq eir-jump-after-eval nil)
      (setq eir-jump-after-eval t)))
  (add-hook 'geiser-mode-hook
            (lambda () (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser)))
  (add-hook 'sh-mode-hook
            (lambda () (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))
  (add-hook 'lisp-mode-hook
            (lambda () (local-set-key (kbd "C-<return>") 'eir-eval-in-slime)))
  (add-hook 'python-mode-hook
            (lambda () (local-set-key (kbd "C-<return>") 'eir-eval-in-python)))
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (local-set-key (kbd "C-<return>") 'eir-eval-in-ielm))))

(use-package beginend
  :defer 5
  :diminish (beginend-bs-mode
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
             beginend-vc-dir-mode)
  :config   (beginend-global-mode))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds '("http://nullprogram.com/feed/"
                       "http://planet.emacsen.org/atom.xml")))

(use-package shift-number
  :load-path "/home/natsu/src/emacs-shift-number"
  :bind (("C-c C-=" . shift-number-up)
         ("C-c C--" . shift-number-down)))

(benchmark-init/show-durations-tabulated)

;; (benchmark-init/show-durations-tree)

;; ucs-normalize


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
     (bug-reference-bug-regexp . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>")))))
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

(provide 'user-init-file)
;;; .emacs ends here
