(package-initialize)

(ffap-bindings)

(winner-mode 1)
(windmove-default-keybindings)

(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
(pinentry-start)

(setq browse-url-mpv-program "mpv")
(setq browse-url-mpv-arguments nil)
(setq python-shell-interpreter "python3")
(setq inhibit-compacting-font-caches t)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'electric-pair-mode)
(add-hook 'minibuffer-inactive-mode-hook 'paredit-mode)
(add-hook 'compilation-mode-hook (lambda () (setq tab-width 13)))
(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)

;; Make eww default for most URLs
;; https://github.com/littlestone/.emacs.d.sai/blob/e47d5cb326ec7ef824dab3ff99dc009349dd93af/settings/my-misc.el
(if (consp browse-url-browser-function)
    (setcdr (assoc "." browse-url-browser-function) 'browse-url-firefox)
  (setq browse-url-browser-function
        `(("^ftp://.*" . browse-ftp-tramp)
          ("youtube" . browse-url-mpv)
	  ("." . browse-url-firefox))))

(global-set-key (kbd "C-c g b") 'guix-switch-to-buffer)
(global-set-key (kbd "C-c g x") 'guix-extended-command)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "<f5>") 'recompile)

(setq guix-directory "~/src/guix")
;; (with-eval-after-load 'geiser-guile
;;   (add-to-list 'geiser-guile-load-path "~/src/guix"))

(defun guix-src-grep (regexp)
  (interactive "sGREP: ")
  (rgrep regexp
	 "*.scm"
	 (concat guix-directory "/gnu/packages")))

(use-package browse-url
  :commands browse-url-mpv
  :config
  (progn
    (setq browse-url-mpv-remote-program "~/bin/mpv-remote")

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
		(list (car (split-string url "&")))))))

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
		(list url)))))))

(use-package cc-mode
  :config
  (add-hook 'c-mode-hook 'electric-pair-mode))

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

(use-package rainbow-identifiers
  :config (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package shell
  :commands shell
  :config (add-hook 'shell-mode-hook 'guix-prettify-mode))

(use-package notmuch
  :bind (("C-c m" . notmuch))
  :commands notmuch)

(use-package erc
  :bind (("C-c e a" . erc-connect-all)
	 ("C-c e f" . erc-connect-freenode)
	 ("C-c e d" . erc-connect-debian)
	 ("C-c e g" . erc-connect-gitter)
	 ("C-c e G" . erc-connect-gnome))
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
	     "##japanese"
	     "#latex"
	     "#python"
	     "#scipy"
	     ;; "#fedora-qa"
	     "#sagemath"
	     "#scheme")
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
    (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
    (setq erc-autojoin-mode t)
    (setq erc-autojoin-timing 'ident)
    (setq erc-server-auto-reconnect t) ;; set to nil if spam
    (setq erc-kill-server-buffer-on-quit nil)
    (setq erc-kill-buffer-on-part nil)
    (setq erc-whowas-on-nosuchnick t) ;; if nick is unknown, use whowas
    (setq erc-timestamp-format "%H:%M ")
    (setq erc-hide-timestamps t)
    (setq erc-server-reconnect-timeout 60)
    (setq erc-server-send-ping-timeout 180)
    (setq erc-server-send-ping-interval 45)
    (setq erc-accidental-paste-threshold-seconds 0.5)
    (setq erc-nick-uniquifier nil)
    (setq erc-server-reconnect-attempts t) ; Always reconnect
    (setq erc-flood-protect t)
    (setq erc-log-insert-log-on-open t)
    (setq erc-track-position-in-mode-line 't)
    (setq erc-track-switch-direction 'importantce)
    (setq erc-track-faces-priority-list '(erc-current-nick-face erc-keyword-face))
    (setq erc-track-priority-faces-only 'all)
    (setq erc-join-buffer 'bury)

    (eval-after-load 'erc
      '(progn
	 (erc-track-mode t)
	 (erc-log-mode)
	 (require 'erc-fill)
	 (erc-fill-mode t)))

    (setq erc-networks-alist nil)

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
  :config (add-hook 'prog-mode-hook 'company-mode))

(use-package hl-todo
  :config
  (progn
    (add-hook 'latex-mode-hook 'hl-todo-mode)
    (add-hook 'prog-mode-hook 'hl-todo-mode)))

(use-package yasnippet
  :config
  (progn
    (yas-reload-all)
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'latex-mode-hook 'yas-minor-mode)))

(use-package dired
  :commands dired-mode
  :config
  (progn
    (add-hook 'dired-mode-hook 'guix-prettify-mode)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))

(use-package elisp-mode
  :commands elisp-mode
  :config (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package scheme
  :commands scheme-mode
  :config
  (progn
    (add-hook 'scheme-mode-hook 'paredit-mode)
    (add-hook 'scheme-mode-hook 'guix-devel-mode)
    (setq geiser-active-implementations (quote (guile)))))

(use-package magit
  :bind (("C-c g s" . magit-status)
	 ("C-c g p" . magit-dispatch-popup)
	 ("C-c g l" . magit-list-repositories)))

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
       (python . t)
       (ipython . t)))
    (setq org-babel-python-command python-shell-interpreter)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))))

(use-package shell
  :bind (("C-c s" . shell)))

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
  :commands geiser-repl-mode
  :config (add-hook 'geiser-repl-mode-hook 'paredit-mode))

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
  :commands google-translate-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-input-method "russian-computer")
 '(display-time-mode t)
 '(magit-auto-revert-mode nil)
 '(magit-repository-directories (quote (("~/src/math" . 0) ("~/src/guix" . 0))))
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
     (:name "my-email" :query "from:go.wigust@gmail.com"))))
 '(org-agenda-files (quote ("~/.notes")))
 '(safe-local-variable-values
   (quote
    ((eval when
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
