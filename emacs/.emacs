(package-initialize)

(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))

(setq user-mail-address "go.wigust@gmail.com")
(setq user-full-name "Oleg Pykhalov")

(setq inhibit-compacting-font-caches t)

(defvar my-projects
  (directory-files (expand-file-name "/srv/git")
                   t
                   "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))


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
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(geiser-guile-binary "guile")
 '(gitpatch-mail-database (quote ("guix-patches@gnu.org")))
 '(ido-mode (quote buffer) nil (ido))
 '(imaxima-scale-factor 1.5)
 '(magit-auto-revert-mode nil)
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
     (:name "pykhalov-unread" :query "from:pykhalov@gmail.com tag:unread")
     (:name "artgames" :query "tag:unread subject:ArtGames LP")
     (:name "my-email" :query "from:go.wigust@gmail.com"))))
 '(org-agenda-files (quote ("~/.notes")))
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
    (add-hook 'Info-mode-hook
              (lambda ()
                (mapcar (lambda (word-font)
                          (font-lock-add-keywords
                           nil `(,(concat "\\<\\(" (car word-font) "\\)") 1
                                 ,(cdr word-font) t)))
                        highlight-words-list)))))

(use-package rainbow-mode
  :bind (("C-c t r" . rainbow-mode)))

(use-package rainbow-identifiers
  :config (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

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

(use-package dired
  :commands dired-mode
  :config
  (progn
    (defun my-dired-mode-hook ()
      (turn-on-gnus-dired-mode)
      (dired-async-mode t)
      (setq-local truncate-lines t))
    (add-hook 'dired-mode-hook 'my-dired-mode-hook)))

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
      (emacs-lisp-mode))))

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
  :bind (("C-c t p" . electric-pair-mode))
  :config
  (progn
    (add-hook 'c-mode-hook 'electric-pair-mode)
    (add-hook 'python-mode-hook 'electric-pair-mode)))

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
  :config
  (progn
    (setq python-shell-interpreter "python3")))

(use-package savehist-mode
  :config (savehist-mode t))

(use-package save-place-mode
  :config (save-place-mode t))

(use-package shell
  :bind(("C-c s s" . shell)
         ("C-c s e" . eshell))
  :config (add-hook 'shell-mode-hook 'guix-prettify-mode))

(use-package whitespace
  :bind (("C-c t w" . whitespace-mode)))

(use-package calendar
  :commands calendar
  :config
  (progn
    (setq calendar-date-style 'european
          calendar-week-start-day 1)))

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
  :config (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))

(use-package imenu
  :bind (("C-c i" . imenu)))


;;;
;;; Tags
;;;

(use-package semantic
  :config
  (progn
    (add-hook 'c-mode-hook 'semantic-mode)
    (add-hook 'c-mode-hook 'semantic-idle-summary-mode)))

(use-package semantic/util-modes
  :after semantic
  :config
  (progn
    (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
    (add-hook 'c-mode-hook 'semantic-stickyfunc-mode)))

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

(use-package helm
  :config (setq helm-locate-project-list my-projects))

(use-package company
  :diminish company-mode
  :config (add-hook 'prog-mode-hook 'company-mode))

(use-package company-quickhelp
  :after company
  :config
  (eval-after-load 'company
    '(define-key company-active-map
       (kbd "C-c h") #'company-quickhelp-manual-begin)))

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
    (add-hook 'latex-mode-hook 'yas-minor-mode)))


;;;
;;; Email
;;;

(use-package gnus
  :bind (("C-c m g" . gnus))
  :config (add-hook 'message-sent-hook 'gnus-score-followup-thread))

(use-package notmuch
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
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . nil)
       (R . t)
       (python . t)))
    (setq org-babel-python-command python-shell-interpreter)
    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale 1.5))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
    (setq org-capture-templates '(("c" "Note" entry
                                   (file "~/org/notes.org")
                                   "* %T %?")
                                  ("w" "Web site" entry
                                   (file "~/.web.org")
                                   "* %a :website:\n\n%U %?\n\n%:initial")
                                  ("r" "Respond ro email" entry
                                   (file+headline
                                    (concat org-directory "/inbox.org") "Email")
                                   "* REPLY to [[mailto:%:fromaddress][%:fromname]] on %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%U\n\n"
                                   :immediate-finish t
                                   :prepend t)
                                  ("f" "File email" entry
                                   (file+headline
                                    (concat org-directory "/inbox.org") "Email")
                                   "* %U %a by [[mailto:%:fromaddress][%:fromname]]\n\n%i%?\n"
                                   :immediate-finish nil
                                   :prepend nil)
                                  ("t" "Task" entry
                                   (file+headline
                                    (concat org-directory "/tasks.org") "Tasks")
                                   "* TODO %? \n%T" :prepend t)))
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
           (eq (magit-section-type section) 'unpushed)
           (equal (magit-section-value section) "@{upstream}..")
           'hide))

    (add-hook 'magit-section-set-visibility-hook
              'local-magit-initially-hide-unmerged)

    (setq magit-repository-directories my-projects)))

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
  (defvar scheme-prettify-symbols-alist
    '(("lambda" . 955)))
  :config
  (progn
    (defun my-scheme-mode-hook ()
      (setq-local prettify-symbols-alist scheme-prettify-symbols-alist)
      (prettify-symbols-mode)
      (show-paren-mode))
    (add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
    (setq geiser-active-implementations (quote (guile)))))

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

(use-package guix-utils
  :after org
  :config
  (progn
    (setq guix-find-file-function 'org-open-file)))

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
    (add-hook 'dired-mode-hook 'guix-prettify-mode)))

(use-package geiser-guile
  :commands geiser-repl-mode
  :config
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "~/src/guix")))

(use-package geiser-doc)

(use-package slime
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
;;; IRC
;;;


(use-package erc
  :bind (("C-c e l" . erc-connect-localhost)
         ("C-c e a" . erc-connect-all)
         ("C-c e f" . erc-connect-freenode)
         ("C-c e d" . erc-connect-debian)
         ("C-c e g" . erc-connect-gitter)
         ("C-c e G" . erc-connect-gnome)
         ("C-c e t" . erc-connect-twitch))
  :init
  (progn
    (setq erc-accidental-paste-threshold-seconds 0.5)
    (setq erc-autojoin-mode t)
    (setq erc-autojoin-timing (quote ident))
    (setq erc-email-userid "go.wigust@gmail.com")
    (setq erc-flood-protect t)
    (setq erc-hide-timestamps t)
    (setq erc-join-buffer (quote bury))
    (setq erc-kill-buffer-on-part nil)
    (setq erc-kill-server-buffer-on-quit nil)
    (setq erc-log-insert-log-on-open t)
    (setq erc-modules (quote (autojoin button completion fill irccontrols list
                                       log match menu move-to-prompt netsplit
                                       networks noncommands readonly ring
                                       smiley stamp track)))
    (setq erc-networks-alist nil)
    (setq erc-nick-uniquifier nil)
    (setq erc-server-auto-reconnect t)
    (setq erc-server-reconnect-attempts t)
    (setq erc-server-reconnect-timeout 60)
    (setq erc-track-position-in-mode-line (quote t))
    (setq erc-track-priority-faces-only (quote all))
    (setq erc-track-switch-direction (quote importantce))
    (setq erc-try-new-nick-p nil)
    (setq erc-user-full-name "Oleg Pykhalov")
    (setq erc-whowas-on-nosuchnick t)
    (setq erc-track-exclude-types '("NICK" "333" "353" "JOIN" "QUIT" "PART"))

    (defun erc-connect-localhost ()
      "Connect to localhost irc network"
      (interactive)
      (erc :server "localhost"
           :port 6667
           :nick "natsu"
           :password nil))

    (defun erc-connect-twitch ()
      "Connect to twitch irc network"
      (interactive)
      (add-to-list 'erc-networks-alist '(twitch "irc.chat.twitch.tv"))
      (erc-tls :server "irc.chat.twitch.tv"
               :port 6697
               :nick "wigust"
               :password nil))

    (defun erc-connect-globalgamers ()
      "Connect to globalgamers irc network"
      (interactive)
      (add-to-list 'erc-networks-alist '(globalgamers "irc.globalgamers.net"))
      (erc-tls :server "irc.globalgamers.net"
               :port 6660
               :nick "wigust"
               :password nil))

    (defun erc-connect-indymedia ()
      "Connect to indymedia irc network"
      (interactive)
      (add-to-list 'erc-networks-alist '(indymedia "irc.indymedia.org"))
      (erc-tls :server "irc.indymedia.org"
               :port 6697
               :nick "wigust"
               :password nil))

    (defun erc-connect-gitter ()
      "Connect to gitter irc network"
      (interactive)
      (add-to-list 'erc-networks-alist '(gitter "irc.gitter.im"))
      (erc-tls :server "irc.gitter.im"
               :port 6697
               :nick "wigust"
               :password nil))

    (defun erc-connect-gnome ()
      "Connect to gnome irc network"
      (interactive)
      (erc-tls :server "irc.gnome.org"
               :port 6697
               :nick "wigust"))

    (defun erc-connect-freenode ()
      "Connect to freenode irc network"
      (interactive)
      (erc-tls :server "irc.freenode.net"
               :port 6697
               :nick "wigust"
               :password nil))

    (defun erc-connect-debian ()
      "Connect to debian irc network"
      (interactive)
      (erc-tls :server "irc.oftc.net"
               :port 6697
               :nick "wigust"))

    (defun erc-connect-rizon ()
      "Connect to highway irc network"
      (interactive)
      (erc-tls :server "irc.rizon.net"
               :port 6697
               :nick "wigust"))

    (defun erc-connect-highway ()
      "Connect to highway irc network"
      (interactive)
      (erc-tls :server "irc.irchighway.net"
               :port 6697
               :nick "wigust"))

    (defun erc-connect-all ()
      "Connect to all configured irc networks"
      (interactive)
      (erc-connect-localhost) (erc-connect-debian)
      (erc-connect-freenode) (erc-connect-gnome)
      (erc-connect-gitter) (erc-connect-twitch)
      (erc-connect-rizon) (erc-connect-globalgamers)
      ;; (erc-connect-highway) ; No autojoin channels
      (erc-connect-indymedia))

    (defvar irc-gnome-servers '("umu.se" "gimp.net" "gimp.ca"
                                "gnome.org" "y.se" "poop.nl"))

    (defvar irc-gnome-channels '("#bugs" "#docs" "#gnome" "#gnome-hackers"
                                 "#gnome-shell" "#newcomers"))

    (defun irc-netlist (irc-networks irc-channels)
      (let (irc-netlist)
        (dolist (irc-network irc-networks irc-netlist)
          (if (equal irc-netlist nil)
              (setq irc-netlist
                    (list (cons irc-network irc-channels)))
            (setq irc-netlist (append
                               irc-netlist
                               (list (cons irc-network irc-channels))))))))

    (defvar irc-netlist-gnome (irc-netlist irc-gnome-servers
                                           irc-gnome-channels))

    (setq erc-autojoin-channels-alist
          (quote
           (("freenode.net" "#icecat" "#emacs" "##math"
             ;; "##c" ;; "#clojure"
             ;; "#fedora" ;; "#fedora-admin" ;; "#fedora-devel"
             ;; "#fedora-noc" ;; "#fedora-meeting" ;; "#fedora-qa"
             "#gnu" "#guile" "#guix"
             ;; "#nixos" ;; "#grub" ;; "#haskell" ;; "#xmonad"
             ;; "#filmsbykris" ;; "##japanese" ;; "#latex"
             ;; "#python" ;; "#scipy" ;; "#sagemath"
             "#scheme")
            ("indymedia.org" "#riseup")
            ("gitter.im")
            ("oftc.net" "#debian" "#debian-next")
            ("globalgamers" "#Touhou")
            ("twitch.tv" "#tsoding" "#cattzs" "#retched"
             "#bbsssssssss" "#team_treehouse" "#rw_grim")
            ("uworld.se" "#coalgirls"))))

    (defun erc-netlist (irc-netlist)
      (dolist (irc-net irc-netlist)
        (append erc-autojoin-channels-alist irc-net)))

    (setq erc-autojoin-channels-alist
          (append erc-autojoin-channels-alist irc-netlist-gnome)))
  :config
  (progn
    (eval-after-load 'erc
      '(progn
         (erc-track-mode t)
         (erc-log-mode)
         (require 'erc-fill)
         (erc-fill-mode t)))

    (add-hook 'erc-mode-hook
              '(lambda ()
                 (require 'erc-pcomplete)
                 (pcomplete-erc-setup)
                 (erc-completion-mode 1)
                 (erc-ring-mode 1)
                 (setq pcomplete-ignore-case t)))
    (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
    ;; bug#18527: 24.3; ERC does not reconnect when server disconnects me
    ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-12/msg01414.html
    (add-hook 'erc-disconnected-hook
              #'(lambda (nick host-name reason)
                  ;; Re-establish the connection even if the server closed it.
                  (setq erc-server-error-occurred nil)))))

(use-package erc-hl-nicks
  :after erc)


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
  :disabled
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

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :bind (("C-c t a" . aggressive-indent-mode)))

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
            ("." . browse-url-default-browser)))

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
  (add-to-list 'browse-url-browser-function
               '("^https?://debbugs\\.gnu\\.org/.*" . debbugs-browse-url)))

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
          ("flv" . "mpv")))))

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
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))))

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
  :config (add-hook 'prog-mode-hook 'flyspell-mode))

(use-package sr-speedbar
  :bind (("C-c s b" . sr-speedbar-toggle)))

(use-package prog-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'prettify-symbols-mode)))
