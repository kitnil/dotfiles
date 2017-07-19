(package-initialize)

(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))

(setq inhibit-compacting-font-caches t)

(use-package ibuffer
  :bind (("C-c b" . ibuffer)))

(use-package compile
  :bind ("<f5>" . recompile))

(use-package server
  :config
  (progn
    (unless (server-running-p)
      (server-start))))

(use-package elec-pair
  :bind (("C-c t p p" . electric-pair-mode))
  :config
  (progn
    (add-hook 'c-mode-hook 'electric-pair-mode)
    (add-hook 'python-mode-hook 'electric-pair-mode)))

(use-package paren
  :bind (("C-c t p m" . show-paren-mode)))

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

(use-package paredit
  :diminish paredit-mode
  :config
  (progn
    (add-hook 'scheme-mode-hook 'paredit-mode)
    (add-hook 'minibuffer-inactive-mode-hook 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)))

(use-package multiple-cursors
  :config (multiple-cursors-mode))

(use-package tex-mode
  :config (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))

(use-package imenu
  :bind (("C-c i" . imenu)))

(use-package browse-url
  :commands browse-url-mpv
  :config
  (progn
    (setq browse-url-mpv-program "mpv")
    (setq browse-url-mpv-arguments nil)
    (setq browse-url-mpv-remote-program "~/bin/mpv-remote")
    (if (consp browse-url-browser-function)
	(setcdr (assoc "." browse-url-browser-function) 'browse-url-firefox)
      (setq browse-url-browser-function
	    `(("^ftp://.*" . browse-ftp-tramp)
	      ("youtube" . browse-url-mpv)
	      ("." . browse-url-firefox))))

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
  :bind (("C-c u" . undo-tree-visualize))
  :config (add-hook 'prog-mode-hook 'undo-tree-mode))

(use-package projectile-global-mode
  :init (projectile-global-mode))

(use-package rainbow-mode
  :bind (("C-c t c r" . rainbow-mode)))

(use-package rainbow-identifiers
  :config (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package shell
  :commands shell
  :config (add-hook 'shell-mode-hook 'guix-prettify-mode))

(use-package gnus
  :bind (("C-c m g" . gnus)))

(use-package notmuch
  :bind (("C-c m n" . notmuch)))

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
    (add-hook 'dired-mode-hook 'guix-prettify-mode)))

(use-package erc
  :bind (("C-c e a" . erc-connect-all)
	 ("C-c e f" . erc-connect-freenode)
	 ("C-c e d" . erc-connect-debian)
	 ("C-c e g" . erc-connect-gitter)
	 ("C-c e G" . erc-connect-gnome)
	 ("C-c e t" . erc-connect-twitch))
  :init
  (progn
    (defun erc-connect-twitch ()
      "Connect to twitch irc network"
      (interactive)
      (add-to-list 'erc-networks-alist '(twitch "irc.chat.twitch.tv"))
      (erc-tls :server "irc.chat.twitch.tv"
	       :port 6697
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
      (erc-connect-debian)
      (erc-connect-freenode)
      (erc-connect-gnome)
      (erc-connect-gitter)
      (erc-connect-twitch)
      (erc-connect-rizon))

    
    (defvar irc-gnome-servers '("umu.se" "gimp.net" "gimp.ca" "gnome.org" "y.se" "poop.nl"))

    (defvar irc-gnome-channels '("#bugs" "#docs" "#gnome" "#gnome-hackers" "#gnome-shell" "#newcomers"))

    (defun irc-netlist (irc-networks irc-channels)
      (let (irc-netlist)
	(dolist (irc-network irc-networks irc-netlist)
	  (if (equal irc-netlist nil)
	      (setq irc-netlist
		    (list (cons irc-network irc-channels)))
	    (setq irc-netlist (append irc-netlist
				      (list (cons irc-network irc-channels))))))))

    (defvar irc-netlist-gnome (irc-netlist irc-gnome-servers irc-gnome-channels))

    (setq erc-autojoin-channels-alist
	  (quote
	   (("freenode.net"
	     "##c"
	     "#clojure"
	     "##math"
	     "#icecat"
	     "#emacs"
	     ;; "#fedora"
	     ;; "#fedora-admin"
	     ;; "#fedora-devel"
	     ;; "#fedora-noc"
	     ;; "#fedora-meeting"
	     "#gnu"
	     "#guile"
	     "#guix"
	     "#grub"
	     "#filmsbykris"
	     "##japanese"
	     "#latex"
	     "#python"
	     "#scipy"
	     ;; "#fedora-qa"
	     "#sagemath"
	     "#scheme")
	    ("indymedia.org"
	     "#riseup")
	    ("gitter.im")
	    ("oftc.net"
	     "#debian"
	     "#debian-next")
	    ("twitch.tv"
	     "#cattzs"
	     "#retched"
	     "#bbsssssssss"
	     "#team_treehouse"
	     "#rw_grim")
	    ("uworld.se"
	     "#coalgirls"))))

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

(use-package expand-region
  :bind (("C-c SPC" . er/expand-region)))

(use-package company
  :diminish company-mode
  :config (add-hook 'prog-mode-hook 'company-mode))

(use-package hl-todo
  :config
  (progn
    (add-hook 'latex-mode-hook 'hl-todo-mode)
    (add-hook 'prog-mode-hook 'hl-todo-mode)))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    (yas-reload-all)
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'latex-mode-hook 'yas-minor-mode)))

(use-package dired
  :commands dired-mode
  :config
  (progn
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))

(use-package elisp-mode
  :config (add-hook 'emacs-lisp-mode-hook 'show-paren-mode))

(use-package scheme
  :commands scheme-mode
  :config
  (progn
    (setq indent-tabs-mode nil)
    (add-hook 'scheme-mode-hook 'guix-devel-mode)
    (add-hook 'scheme-mode-hook 'show-paren-mode)
    (setq geiser-active-implementations (quote (guile)))))

(use-package proced
  :commands proced
  :config (add-hook 'proced-mode 'guix-prettify-mode))

(use-package magit
  :bind (("C-c v s" . magit-status)
	 ("C-c v p" . magit-dispatch-popup)
	 ("C-c v l" . magit-list-repositories)
	 ("C-c v v" . magit-stage)))

(use-package savehist-mode
  :config (savehist-mode t))

(use-package save-place-mode
  :config (save-place-mode t))

(use-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode +1))

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
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
    (setq org-todo-keywords
       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))))

(use-package shell
  :bind (("C-c s s" . shell)
	 ("C-c s e" . eshell)))

(use-package calendar
  :commands calendar
  :config
  (progn
    (setq calendar-date-style 'european
	  calendar-week-start-day 1)))

(use-package time
  :commands display-time
  :config (setq display-time-24hr-format t))

(use-package geiser-guile
  :commands geiser-repl-mode)

(use-package info-look)

(use-package geiser-doc)

(use-package info
  :config (info-initialize))

(use-package doc-view)

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (progn
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
    (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
    (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
    (setq TeX-save-query nil)))

(use-package google-translate-mode
  :bind (("C-c t t t" . google-translate-mode)))

(use-package imaxima
  :commands imaxima)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-input-method "russian-computer")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(erc-accidental-paste-threshold-seconds 0.5)
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing (quote ident))
 '(erc-email-userid "go.wigust@gmail.com")
 '(erc-flood-protect t t)
 '(erc-hide-timestamps t)
 '(erc-join-buffer (quote bury))
 '(erc-kill-buffer-on-part nil)
 '(erc-kill-server-buffer-on-quit nil)
 '(erc-log-insert-log-on-open t)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring smiley stamp track)))
 '(erc-networks-alist nil)
 '(erc-nick-uniquifier nil)
 '(erc-server-auto-reconnect t)
 '(erc-server-reconnect-attempts t)
 '(erc-server-reconnect-timeout 60)
 '(erc-server-send-ping-interval 45)
 '(erc-server-send-ping-timeout 180)
 '(erc-timestamp-format "%H:%M ")
 '(erc-track-faces-priority-list (quote (erc-current-nick-face erc-keyword-face)))
 '(erc-track-position-in-mode-line (quote t))
 '(erc-track-priority-faces-only (quote all))
 '(erc-track-switch-direction (quote importantce))
 '(erc-try-new-nick-p nil)
 '(erc-user-full-name "Oleg Pykhalov")
 '(erc-whowas-on-nosuchnick t)
 '(geiser-guile-binary "guile")
 '(imaxima-scale-factor 1.5)
 '(magit-auto-revert-mode nil)
 '(magit-repository-directories
   (quote
    (("~/src/math" . 0)
     ("~/src/guix" . 0)
     ("~/src/emacs-emamux" . 0)
     ("~/src/emacs-org-edit-latex" . 0)
     ("~/src/emacs-dired-hacks" . 0)
     ("~/src/emacs-which-key" . 0)
     ("~/src/guile" . 0))))
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
     (:name "leti-user" :query "from:leti-user@googlegroups.com")
     (:name "patch-today" :query "subject:patch date:today")
     (:name "youtube" :query "from:noreply@youtube.com")
     (:name "youtube-today" :query "from:noreply@youtube.com date:today")
     (:name "youtube-unread" :query "from:noreply@youtube.com tag:unread")
     (:name "headhunter-unread" :query "from:no_reply@hh.ru tag:unread")
     (:name "pykhalov-unread" :query "from:pykhalov@gmail.com tag:unread")
     (:name "artgames" :query "tag:unread subject:ArtGames LP")
     (:name "my-email" :query "from:go.wigust@gmail.com")
     (:name "family" :query "from:pykhalov@gmail.com from:gitarika@rambler.ru tag:unread"))))
 '(org-agenda-files (quote ("~/.notes")))
 '(safe-local-variable-values
   (quote
    ((Base . 10)
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
 '(shr-width 80)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
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
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 143 :width normal)))))
(put 'narrow-to-region 'disabled nil)
