;; Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

;; Tip: "M-x e" on `(emacs-init-time)'.

;; Prettify without breaking indentation,
;; Origin <http://endlessparentheses.com/using-prettify-symbols-in-clojure-and-elisp-without-breaking-indentation.html>.

;; Prevent stale elisp bytecode from shadowing more up-to-date source
;; files.  Origin <https://github.com/technomancy/better-defaults>.
(setq load-prefer-newer t)

;; (require 'benchmark-init)

;; To disable collection of benchmark data after init is done.
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(setq package-archives nil) ; Makes unpure packages archives unavailable

(setq user-mail-address "go.wigust@gmail.com")
(setq user-full-name "Oleg Pykhalov")
(setq default-input-method "russian-computer") ; C-\ will switch keyboard layout

(setq display-time-24hr-format t) ; No AM/PM
(setq calendar-date-style 'european) ; day/month/year style calendar
(setq calendar-week-start-day 1) ; Monday is the first day of the week

(setq initial-buffer-choice t) ; Nothing after starting Emacs
(setq initial-scratch-message nil) ; Don't put text in *scratch* buffer

(setq smtpmail-queue-mail t) ; Call after typing M-x `smtpmail-send-queued-mail'

(menu-bar-mode -1)
(scroll-bar-mode 1)
(set-scroll-bar-mode 'right)

;; Default from Emacs 26
;; See <http://git.savannah.gnu.org/cgit/emacs.git/commit/etc/NEWS?id=72ee93d68daea00e2ee69417afd4e31b3145a9fa>
(setq print-quoted t)

;; TODO: Maybe remove
(add-to-list 'exec-path (expand-file-name "~/.guix-profile.d/gdb/bin"))

(defvar ‎wi-headphones "pulse/alsa_output\
.usb-Logitech_Logitech_USB_Headset-00.analog-stereo"
  "My USB headphones")

;; TODO: Make initialization without require
(use-package org-protocol :defer 5)
(use-package jl-encrypt :defer 5)
(use-package crux :defer 5)

(use-package notmuch
  :commands notmuch-search
  :config
  ;; See <https://notmuchmail.org/pipermail/notmuch/2014/019797.html>.
  (setq mail-user-agent 'gnus-user-agent))

;; FIXME: Doesn't load on start up
(use-package undo-tree-mode :defer 5 :config (global-undo-tree-mode))

(use-package projectile :defer 5 :config (projectile-global-mode))

;; TODO: Lazy load
(use-package debbugs-browse) ; for debbugs-browse-url

(setq browse-url-browser-function
      `(("^ftp://.*" . browse-ftp-tramp)
        (,debbugs-browse-url-regexp . debbugs-browse-url)
        ("^https?://w*\\.?youtube.com/watch\\?v=.*" . browse-url-mpv)
        ("." . browse-url-conkeror)))

;; Enable functions
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)


;;;
;;; Keybindings
;;;
;;; See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html>.

(bind-key "<Scroll_Lock>" #'scroll-lock-mode)
(bind-key "<C-mouse-4>"   #'text-scale-increase)
(bind-key "<C-mouse-5>"   #'text-scale-decrease)

(which-key-add-key-based-replacements "C-c &" "yasnippet")

(bind-keys :prefix "C-c q q" :prefix-map wi-q-map :which "foo"
           ("b" . ibuffer)
           ("e" . wi-switch-to-scratch-elisp)
           ("w" . wi-switch-to-eww))

(bind-keys :prefix "C-c b" :prefix-map wi-buffer-map :which "buffer"
           ("b" . ibuffer)
           ("e" . wi-switch-to-scratch-elisp)
           ("w" . wi-switch-to-eww))

(bind-key "<C-down-mouse-1>" 'mc/toggle-cursor-on-click)

(bind-keys :prefix "C-c a" :prefix-map wi-text-map :which "text"
           ("a" . align-regexp)
           ("p" . wi-mark-paragraph+sort-lines)
           ("s" . wi-sort-sexps))

(bind-keys :prefix "C-c a e" :prefix-map wi-expand-map :which "expand"
           ("e" . er/expand-region)
           ("q" . er/mark-inside-pairs)
           ("Q" . er/mark-outside-pairs)
           ("p" . er/mark-inside-pairs)
           ("P" . er/mark-outside-pairs)
           ("m" . er/mark-method-call)
           ("s" . er/mark-symbol)
           ("w" . er/mark-word))

(bind-keys :prefix "C-c o" :prefix-map wi-split-map :which "split"
           ("c" . crux-open-with)
           ("j" . sp-join-sexp)
           ("s" . sp-split-sexp))

(bind-keys :prefix "C-c w" :prefix-map wi-word-map :which "word"
           ("t" . show-translation))

(bind-keys :prefix "C-c v" :prefix-map wi-version-control-map
           :which "version-control"
           ("p" . git-messenger:popup-message))

(bind-keys :prefix "C-c v m" :prefix-map wi-magit-map :which "magit"
           ("c" . magit-commit)
           ("l" . magit-list-repositories)
           ("r" . magit-diff-toggle-refine-hunk)
           ("s" . magit-status))

(bind-keys :prefix "C-c v b" :prefix-map wi-browse-at-remote-map
           :which "browse-at-remote"
           ;; TODO: ("g" . wi-browse-at-remote-gnu)
           ("b" . browse-at-remote))

(which-key-add-key-based-replacements "C-c v h" "version-control-hunk")
(bind-keys :prefix "C-c v h" :prefix-map wi-version-control-hunk-map
           :which "version-control-hunk"
           ("s" . git-gutter:stage-hunk)
           ("r" . git-gutter:revert-hunk))

(bind-keys :prefix "C-c f" :prefix-map wi-find-map :which "find"
           ("d" . dumb-jump-go)
           ("e" . guix-edit)
           ("f" . ffap)
           ("l" . recentf-open-files)
           ("r" . ffap-read-only))

(bind-keys :prefix "C-c f b" :prefix-map wi-browse-map :which "browse"
           ("c" . browse-url-conkeror)
           ("e" . eww)
           ("g" . browse-url-chromium)
           ("i" . browse-url-firefox)
           ("m" . browse-url-mpv))

(bind-keys :prefix "C-c t" :prefix-map wi-toggle-map :which "toggle"
           ("a" . abbrev-mode)
           ("b" . guix-build-log-minor-mode)
           ("c" . company-mode)
           ("e" . prettify-symbols-mode)
           ("f" . flycheck-mode)
           ("g" . guix-prettify-mode)
           ("h" . hl-line-mode)
           ("i" . aggressive-indent-mode)
           ("l" . smartparens-strict-mode)
           ("m" . flymake-mode)
           ("p" . projectile-global-mode)
           ("q" . highlight-stages-mode)
           ("s" . flyspell-mode)
           ("t" . toggle-truncate-lines)
           ("v" . google-translate-mode)
           ("w" . whitespace-mode)
           ("y" . yas-minor-mode))

(bind-keys :prefix "C-c e" :prefix-map wi-expand-map :which "expand"
           ("w"  . er/mark-word)
           ("s"  . er/mark-symbol)
           ("S"  . er/mark-symbol-with-prefix)
           ("n"  . er/mark-next-accessor)
           ("m"  . er/mark-method-call)
           ("'"  . er/mark-inside-quotes)
           ("\"" . er/mark-outside-quotes)
           ("p"  . er/mark-inside-pairs)
           ("P"  . er/mark-outside-pairs)
           ("c"  . er/mark-comment)
           ("u"  . er/mark-url)
           ("e"  . er/mark-email)
           ("d"  . er/mark-defun))

(bind-keys :prefix "C-c r" :prefix-map wi-rething-map :which "rething"
           ("r" . revert-buffer)
           ("l" . redraw-display)
           ("f" . transpose-frame)
           ("w" . transpose-frame))

(bind-keys :prefix "C-c r t" :prefix-map wi-transpose-map :which "transpose"
           ("f" . transpose-frame)
           ("w" . crux-transpose-windows))

(bind-keys :prefix "C-c h" :prefix-map wi-helm-map :which "helm"
           ("&" . helm-yas-complete)
           ("a" . helm-world-time)
           ("b" . helm-buffers-list)
           ("e" . helm-emms)
           ("f" . helm-for-files)
           ("i" . helm-imenu)
           ("m" . helm-make)
           ("l" . helm-recentf)
           ("r" . helm-bookmarks)
           ("s" . helm-pass)
           ("t" . helm-top)
           ("v" . helm-wigust-stream)
           ("w" . helm-stumpwm-commands)
           ("x" . helm-M-x)
           ("y" . helm-show-kill-ring))

(bind-keys :prefix "C-c h h" :prefix-map wi-helm-help-map :which "help"
           ("m" . helm-man-woman)
           ("i" . helm-info))

(bind-keys :prefix "C-c i" :prefix-map wi-ivy-map :which "ivy"
           ("b" . ivy-switch-buffer)
           ("f" . counsel-find-file)
           ("l" . ivy-recentf)
           ("r" . ivy-resume)
           ("s" . swiper)
           ("x" . counsel-M-x))

(bind-keys :prefix "C-c i g" :prefix-map wi-counsel-git-map
           :which "counsel-git"
           ("f" . counsel-git)
           ("v" . counsel-git-grep))

(bind-keys :prefix "C-c i h" :prefix-map wi-counsel-help-map
           :which "counsel-help"
           ("f" . counsel-describe-function)
           ("i" . counsel-info-lookup-symbol)
           ("l" . counsel-find-library)
           ("u" . counsel-unicode-char)
           ("v" . counsel-describe-variable))

(which-key-add-key-based-replacements "C-c p x" "projectile-shell")
(which-key-add-key-based-replacements "C-c p s" "projectile-search")
(bind-keys :prefix "C-c h p" :prefix-map wi-helm-projectile-map
           :which "helm-projectile"
           ("p" . helm-projectile)
           ("f" . helm-projectile-find-file-dwim)
           ("b" . helm-projectile-switch-to-buffer))

(bind-keys :prefix "C-c m" :prefix-map wi-mail-map :which "mail"
           ("b" . wi-send-buffer-as-mail))

(bind-keys :prefix "C-c e" :prefix-map wi-emms-map :which "emms"
           ("e" . emms)
           ("n" . emms-next)
           ("p" . emms-previous)
           ("r" . emms-random)
           ("s" . emms-stop))

(bind-keys :prefix "C-c m d" :prefix-map wi-debbugs-map :which "debbugs"
           ("b" . debbugs-gnu-bugs)
           ("l" . debbugs-gnu)
           ("p" . debbugs-gnu-patches)
           ("s" . debbugs-gnu-search)
           ("u" . wi-debbugs-gnu-list))

(bind-keys :prefix "C-c m r" :prefix-map wi-elfeed-map :which "elfeed"
           ("r" . elfeed)
           ("g" . elfeed-update))

(bind-keys :prefix "C-c m g" :prefix-map wi-gnus-map :which "gnus"
           ("g" . gnus)
           ("s" . switch-to-gnus))

(bind-keys :prefix "C-c s" :prefix-map wi-shell-map :which "shell"
           ("s" . shell)
           ("c" . compilation-shell-minor-mode)
           ("e" . eshell)
           ("h" . wi-terminal-here-launch)
           ("t" . term))

(bind-keys :prefix "C-c c" :prefix-map wi-org-map :which "org"
           ("c" . org-capture)
           ("a" . org-agenda)
           ("l" . org-store-link))

(which-key-add-key-based-replacements "C-c k" "engine")

(bind-key "<f5>" #'recompile)
(bind-key "<f6>" #'god-local-mode)
(bind-key "<f7>" #'mc/mark-next-like-this)
(bind-key "<f8>" #'er/expand-region)
(bind-key "<M-f6>" #'god-mode-all)
(bind-key "M-z" #'zap-up-to-char)
(bind-key "C-c u" #'undo-tree-visualize)

; TODO: (bind-key "<C-tab>" #'hs-toggle-hiding scheme-mode-map)

(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".mbsyncrc" . conf-mode))
(add-to-list 'auto-mode-alist '(".conkerorrc" . js-mode))
(add-to-list 'auto-mode-alist '("manifest" . scheme-mode))

(add-hook 'scheme-mode-hook
          (lambda () (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser)))

(add-hook 'sh-mode-hook
          (lambda () (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))

(add-hook 'lisp-mode-hook
          (lambda () (local-set-key (kbd "C-<return>") 'eir-eval-in-slime)))

(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-<return>") 'eir-eval-in-python)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (local-set-key (kbd "C-<return>") 'eir-eval-in-ielm)))


;;;
;;; Search engines
;;;

(with-eval-after-load 'engine-mode
  (setq engine/keybinding-prefix "C-c k")
  (engine/set-keymap-prefix (kbd engine/keybinding-prefix))
  (engine-mode))

(defengine arch-packages
  "https://www.archlinux.org/packages/?sort=&q=%s")

(defengine cpan
  "http://search.cpan.org/search?query=%s&mode=all")

(defengine cve
  "https://cve.mitre.org/cgi-bin/cvekey.cgi?keyword=%s")

(defengine debfiles
  "https://packages.debian.org/search?searchon=contents&keywords=%s")

(defengine debcodesearch
  "https://codesearch.debian.net/search?q=%s")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine explainshell
  "https://www.explainshell.com/explain?cmd=%s")

(defengine debfiles
  "https://packages.debian.org/search?searchon=contents&keywords=%s")

(defengine fdroid
  "https://f-droid.org/packages/#q=%s")

(defengine fedora-cgit
  "https://fedorapeople.org/cgit/?q=%s")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s")

(defengine github-gpl
  (concat "https://github.com/search?ref=simplesearch&q=%s"
          "+license%%3Agpl"))

(defengine github-hippie
  (concat "https://github.com/search?ref=simplesearch&q=%s"
          "+-language:objectivec"
          "+-language:java"
          "+-language:javascript"
          "+-language:csharp"
          "+-language:kotlin"
          "+-language:swift"
          "+-language:php"
          "+-language:vue"
          "+-language:autohotkey"))

(defengine github-hippie-gpl
  (concat "https://github.com/search?ref=simplesearch&q=%s"
          "+-language:objectivec"
          "+-language:java"
          "+-language:javascript"
          "+-language:csharp"
          "+-language:kotlin"
          "+-language:swift"
          "+-language:php"
          "+-language:vue"
          "+-language:autohotkey"
          "+license%%3Agpl"))

(defengine google
  "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

(defengine google-door-music
  ;; https://github.com/gotbletu/dotfiles/blob/66b2ce9744564a48717c97163a5c34ad1b56d50e/surfraw/.config/surfraw/elvi/opendir_music
  (concat "https://www.google.com/search?q=%s"
          "%%20%%2B(.ogg|.mp3|.wav|.ac3|.flac|.wma|.m4a)"
          "%%20%%2Bintitle:%%22index%%20of%%22%%20"
          "-inurl:(jsp|pl|php|html|aspx|htm|cf|shtml)%%20"
          "-inurl:(listen77|mp3raid|mp3toss|mp3drug|index_of|wallywashis)"))

(defengine google-video
  "https://www.google.com/search?q=%s&tbm=vid")

(defengine guix-hydra
  "https://hydra.gnu.org/search?query=%s")

(defengine guix-hydra-job
  ;; e.g. gource-0.47
  "https://hydra.gnu.org/job/gnu/master/%s.x86_64-linux")

(defengine nixos-hydra
  "https://hydra.nixos.org/search?query=%s")

(defengine nixos-hydra-job
  "https://hydra.nixos.org/job/gnu/master/%s.x86_64-linux")

;; TODO: Do more intelegent

(defengine listinfo-gnu
  "https://lists.gnu.org/mailman/listinfo/%s")

(defengine info-gnus-english
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s"
          "&submit=Search%%21"
          "&idxname=info-gnus-english"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine info-gnus-english-message-id
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
          "%%2Bmessage-id%%3A%s"
          "&submit=Search%%21"
          "&idxname=info-gnus-english"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine guix-devel
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s"
          "&submit=Search%%21"
          "&idxname=guix-devel&max=20" "&result=normal" "&sort=score"))

(defengine guix-devel-message-id
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
          "%%2Bmessage-id%%3A%s"
          "&submit=Search%%21"
          "&idxname=guix-devel"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine guix-help
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s"
          "&submit=Search%%21"
          "&idxname=help-guix&max=20" "&result=normal" "&sort=score"))

(defengine guix-help-message-id
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
          "%%2Bmessage-id%%3A%s"
          "&submit=Search%%21"
          "&idxname=help-guix"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine guix-help+devel
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi"
          "?query=%s"
          "&submit=Search%%21"
          "&idxname=guix-devel"
          "&idxname=help-guix"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine guix-all
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi"
          "?query=%s"
          "&submit=Search%%21"
          "&idxname=guix-devel"
          "&idxname=help-guix"
          "&idxname=bug-guix"
          "&idxname=guix-patches"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine guix-all-date
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi"
          "?query=%s"
          "&submit=Search%%21"
          "&idxname=guix-devel"
          "&idxname=help-guix"
          "&idxname=bug-guix"
          "&idxname=guix-patches"
          "&max=20"
          "&result=normal"
          "&sort=date%%3Alate"))

(defengine emacs-devel
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s"
          "&submit=Search%%21"
          "&idxname=emacs-devel&max=20" "&result=normal" "&sort=score"))

(defengine emacs-devel-message-id
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
          "%%2Bmessage-id%%3A%s"
          "&submit=Search%%21"
          "&idxname=emacs-devel"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine help-gnu-emacs
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s"
          "&submit=Search%%21"
          "&idxname=help-gnu-emacs&max=20" "&result=normal" "&sort=score"))

(defengine help-gnu-emacs-message-id
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
          "%%2Bmessage-id%%3A%s"
          "&submit=Search%%21"
          "&idxname=help-gnu-emacs"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine emacs-orgmode
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s"
          "&submit=Search%%21"
          "&idxname=emacs-orgmode&max=20" "&result=normal" "&sort=score"))

(defengine emacs-orgmode-message-id
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
          "%%2Bmessage-id%%3A%s"
          "&submit=Search%%21"
          "&idxname=emacs-orgmode"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine mankier
  "https://www.mankier.com/?q=%s")

(defengine melpa
  "https://melpa.org/#/?q=%s"
  :keybinding "m")

(defengine openhub
  "https://www.openhub.net/p?ref=homepage&query=%s")

(defengine reddit-unixporn
  "https://www.reddit.com/r/unixporn/search?q=%s&restrict_sr=on")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine searx
  "http://searx.tk/?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s")

(defengine startpage
  "https://www.startpage.com/do/search?query=%s"
  :keybinding "s")

(defengine startpage-hippie
  (concat "https://www.startpage.com/do/dsearch?query=%s"
          "+c"
          "+-c%%2B%%2B"
          "+-c%%23&cat=web"
          "&pl=opensearch"
          "&language=english"))

(defengine tldr
  "https://tldr.ostera.io/%s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")

(defengine wiktionary
  (concat "https://www.wikipedia.org/search-redirect.php?family=wiktionary"
          "&language=en" "&go=Go" "&search=%s"))

(defengine metal-archives
  "https://www.metal-archives.com/search?searchString=%s&type=band_name")

(defengine libgen
  (concat "http://libgen.io/search.php?req=%s&"
          "lg_topic=libgen&"
          "open=0&"
          "view=simple&"
          "res=25&"
          "phrase=1&"
          "column=def"))

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s")

(defengine youtube-latest
  "https://www.youtube.com/results?sp=CAJQFA%%253D%%253D&search_query=%s")

(defengine youtube-live
  "https://www.youtube.com/results?sp=EgJAAQ%%253D%%253D&search_query=%s")

(defengine youtube-rss-channel
  "https://www.youtube.com/feeds/videos.xml?channel_id=%s")

(defengine youtube-rss-user
  "https://www.youtube.com/feeds/videos.xml?user=%s")

(defengine webarchive
  "https://web.archive.org/web/*/%s")


;;;
;;; Usability functions
;;;

(defun wi-mark-paragraph+sort-lines ()
  "Invoke `mark-paragraph' and `sort-lines'."
  (interactive)
  (mark-paragraph)
  (sort-lines nil (region-beginning) (region-end)))

;; TODO:
(defmacro wi-define-insert (name-text-list)
  `(mapc (lambda (name-text)
           (let ((name (first name-text))
                 (text (second name-text)))
             (defun ,(intern (concat "wi-insert-" (symbol-name name))) ()
               (interactive)
               (insert text))))
         ,name-text-list))

(defun wi-dunno ()
  (interactive)
  "Insert a `¯\_(ツ)_/¯' thing."
  (insert "¯\_(ツ)_/¯"))

(defun wi-arrow-up ()
  (interactive)
  "Insert a `↑' symbol."
  (insert "↑"))

(defun wi-ellipsis-horizontal ()
  (interactive)
  "Insert a `…' symbol."
  (insert "…"))

(defun wi-guix-download (url)
  (interactive "sDownload URL: ")
  (insert (shell-command-to-string (concat "guix download "
                                           url
                                           " 2>/dev/null"
                                           "| tail -n 1"
                                           "| tr -d '\n'"))))

(defun wi-switch-to-scratch-elisp ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun wi-switch-to-eww ()
  (interactive)
  (switch-to-buffer "*eww*"))

(defun wi-shell-current-dir ()
  (interactive)
  (shell (concat "*shell " default-directory "*")))

(defun close-all-parentheses ()
  (interactive "*")
  (let ((closing nil))
    (save-excursion
      (while (condition-case nil
         (progn
           (backward-up-list)
           (let ((syntax (syntax-after (point))))
             (case (car syntax)
               ((4) (setq closing (cons (cdr syntax) closing)))
               ((7 8) (setq closing (cons (char-after (point)) closing)))))
           t)
           ((scan-error) nil))))
    (apply #'insert (nreverse closing))))

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

;; See <https://lists.gnu.org/archive/html/emacs-devel/2017-12/msg00017.html>.
(defun wi-git-log (&optional repo commit)
  "Check REPO for COMMIT and if it exists, display its commit message.
Interactively, prompt for REPO, defaulting to emacs-master, and
for COMMIT, defaulting to the commit hash at point."
  (interactive "p")
  (let* ((git-dir (if repo
		      (read-directory-name "Repo: " "/mnt/data/steve/git/"
					   nil t "emacs-master")
		    "/mnt/data/steve/git/emacs-master"))
	 (commit0 (or commit (read-string "Commit: " nil nil (word-at-point))))
	 (default-directory git-dir)
	 (output-buffer (get-buffer-create "*git log*"))
	 (proc (progn
		 (with-current-buffer output-buffer (erase-buffer))
		 (call-process "git" nil output-buffer nil
			       "branch" "--contains" commit0))))
    (when proc
      (with-current-buffer output-buffer
	(goto-char (point-min))
	(unless (looking-at "[ *]")
	  (user-error "%s is not on branch %s" commit0
		      (file-name-base git-dir)))
	(insert "Branches:\n")
	(goto-char (point-max))
	(call-process "git" nil output-buffer nil "log" "-1" commit0)
	(pop-to-buffer output-buffer)))))


;;;
;;; Elisp
;;;

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
(add-hook 'elisp-mode-hook (lambda ()
                             (set (make-local-variable 'prettify-symbols-alist)
                                  wi-elisp--prettify-symbols-alist)))

;;;
;;; Guile and Guix
;;;

(with-eval-after-load 'geiser
  (setq geiser-active-implementations (quote (guile chicken)))
  (setq geiser-default-implementation 'guile))

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "/home/natsu/src/guix")
  (setq geiser-guile-binary '("guile" "--no-auto-compile")))

(defconst wi-scheme--prettify-symbols-alist
  '(("lambda" . ?λ)
    ("lambda*" . (?λ (Br . Bl) ?*))))

(add-hooks
 '(((scheme-mode-hook geiser-repl-mode-hook)
    . (lambda ()
        (set (make-local-variable 'prettify-symbols-alist)
             wi-scheme--prettify-symbols-alist)))))

(with-eval-after-load 'guix-repl
  (setq guix-directory (expand-file-name "~/src/guix")))

(setq guix-read-package-name-function #'guix-read-package-name-at-point)


;;;
;;; C-mode
;;;

(defconst wi-c--prettify-symbols-alist
  '((" % " . (? (Br . Bl) ?m
                (Br . Bl) ?o
                (Br . Bl) ?d
                (Br . Bl) ? ))
    (" * " . (? (Br . Bl) ?·
                (Br . Bl) ? ))
    (" / " . (? (Br . Bl) ?÷
                (Br . Bl) ? ))
    ("!" . ?¬)
    ("!=" . ?≢)
    ("&&" . ?∧)
    ("->" . (?  (Br . Bl) ?→
                (Br . Bl) ? ))
    ("<=" . ?≤)
    ("==" . ?≡)
    (">=" . ?≥)
    ("NULL" . ?N)
    ("false" . ?F)
    ("float" . ?ℚ)
    ("int" . ?ℤ)
    ("rand" . ?𝔼)
    ("true" . ?T)
    ("uint32_t" . (?ℕ (Br . Bl) ?₃
                      (Br . Bl) ?₂))
    ("uint8_t" . (?ℕ (Br . Bl) ?₈))
    ("union" . ?∪)
    ("void" . ?Ø)
    ("x_1" . (?x (Br . Bl) ?₁))
    ("x_2" . (?x (Br . Bl) ?₂))
    ("y_1" . (?y (Br . Bl) ?₁))
    ("y_2" . (?y (Br . Bl) ?₂))
    ("||" . ?∨)))

(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'prettify-symbols-alist)
                 wi-c--prettify-symbols-alist)))
(add-hook 'c-mode-hook 'prettify-symbols-mode)

(with-eval-after-load 'cc-vars
  (add-to-list 'c-cleanup-list 'space-before-funcall))

(with-eval-after-load 'semantic
  (global-semantic-decoration-mode t))


;;;
;;; Magit
;;;

(defvar wi-projects-directory (expand-file-name "~/src"))

(defun wi-update-magit-repository-directories (directory)
  "Update list of files in `DIRECTORY' for `magit-list-repositories'."
  (interactive)
  (setq magit-repository-directories (wi-list-files-in-dir directory)))

(setq magit-repository-directories-depth 1)

(wi-update-magit-repository-directories wi-projects-directory)

(setq magit-log-arguments (list "--graph" "--color" "--decorate" "-n64" "--show-signature"))
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

(add-hook 'git-commit-mode-hook 'auto-fill-mode)


;;;
;;; ERC
;;;

(setq erc-accidental-paste-threshold-seconds 0.5)
(setq erc-autojoin-mode t)
(setq erc-autojoin-timing (quote ident))
(setq erc-email-userid user-mail-address)
(setq erc-flood-protect t)
(setq erc-hide-timestamps t)
(setq erc-join-buffer (quote bury))
(setq erc-kill-buffer-on-part nil)
(setq erc-kill-server-buffer-on-quit nil)
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
(setq erc-modules
      '(autojoin button completion fill irccontrols list match menu
        move-to-prompt netsplit networks ring smiley stamp track))
(setq erc-fill-function 'erc-fill-static)

(defun wi-erc-connect-localhost ()
  "Connect to localhost irc network"
  (interactive)
  (erc :server "localhost"
       :port 6667
       :nick "natsu"
       :password nil))

(defun wi-erc-connect-twitch ()
  "Connect to twitch irc network"
  (interactive)
  (add-to-list 'erc-networks-alist '(twitch "irc.chat.twitch.tv"))
  (erc-tls :server "irc.chat.twitch.tv"
           :port 6697
           :nick "wigust"
           :password nil))

(defun wi-erc-connect-globalgamers ()
  "Connect to globalgamers irc network"
  (interactive)
  (add-to-list 'erc-networks-alist '(globalgamers "irc.globalgamers.net"))
  (erc-tls :server "irc.globalgamers.net"
           :port 6660
           :nick "wigust"
           :password nil))

(defun wi-erc-connect-indymedia ()
  "Connect to indymedia irc network"
  (interactive)
  (add-to-list 'erc-networks-alist '(indymedia "irc.indymedia.org"))
  (erc-tls :server "irc.indymedia.org"
           :port 6697
           :nick "wigust"
           :password nil))

(defun wi-erc-connect-gitter ()
  "Connect to gitter irc network"
  (interactive)
  (add-to-list 'erc-networks-alist '(gitter "irc.gitter.im"))
  (erc-tls :server "irc.gitter.im"
           :port 6697
           :nick "wigust"
           :password nil))

(defun wi-erc-connect-gnome ()
  "Connect to gnome irc network"
  (interactive)
  (erc-tls :server "irc.gnome.org"
           :port 6697
           :nick "wigust"))

(defun wi-erc-connect-freenode ()
  "Connect to freenode irc network"
  (interactive)
  (erc-tls :server "irc.freenode.net"
           :port 6697
           :nick "wigust"
           :password nil))

(defun wi-erc-connect-debian ()
  "Connect to debian irc network"
  (interactive)
  (erc-tls :server "irc.oftc.net"
           :port 6697
           :nick "wigust"))

(defun wi-erc-connect-rizon ()
  "Connect to highway irc network"
  (interactive)
  (erc-tls :server "irc.rizon.net"
           :port 6697
           :nick "wigust"))

(defun wi-erc-connect-highway ()
  "Connect to highway irc network"
  (interactive)
  (erc-tls :server "irc.irchighway.net"
           :port 6697
           :nick "wigust"))

(defun wi-erc-connect-all ()
  "Connect to all configured irc networks"
  (interactive)
  (wi-erc-connect-localhost) (wi-erc-connect-debian)
  (wi-erc-connect-freenode) (wi-erc-connect-gnome)
  (wi-erc-connect-gitter) (wi-erc-connect-twitch)
  (wi-erc-connect-rizon) (wi-erc-connect-globalgamers)
  ;; (wi-erc-connect-highway) ; No autojoin channels
  (wi-erc-connect-indymedia))

(defvar wi-irc-gnome-servers '("umu.se" "gimp.net" "gimp.ca"
                               "gnome.org" "y.se" "poop.nl"))

(defvar wi-irc-gnome-channels '("#bugs" "#docs" "#gnome" "#gnome-hackers"
                                "#gnome-shell" "#newcomers"))

(defun wi-erc-netlist (irc-networks irc-channels)
  (let (wi-erc-netlist)
    (dolist (irc-network irc-networks wi-erc-netlist)
      (if (equal wi-erc-netlist nil)
          (setq wi-erc-netlist
                (list (cons irc-network irc-channels)))
        (setq wi-erc-netlist (append
                              wi-erc-netlist
                              (list (cons irc-network irc-channels))))))))

(defvar wi-erc-netlist-gnome (wi-erc-netlist wi-irc-gnome-servers
                                             wi-irc-gnome-channels))

(setq erc-autojoin-channels-alist
      (quote
       (("freenode.net" "#icecat" "#emacs" "#grub" "#conkeror"
         ;; "#clojure" "##math"
         "##c" "#bash" "#SDL" "#chicken"
         ;; "#fedora" "#fedora-admin" "#fedora-devel"
         ;; "#fedora-noc" "#fedora-meeting" "#fedora-qa"
         "#gnu" "#fsf" "#gnus" "#guile" "#guix" "#stumpwm" "#replicant" "#gdb"
         "##linux" "#linuxdistrocommunity"
         ;; "#nixos" "#haskell" "#xmonad"
         ;; "#filmsbykris" "##japanese" "#latex"
         ;; "#python" "#scipy" "#sagemath"
         "#lisp" "#scheme")
        ("indymedia.org" "#riseup")
        ("gitter.im")
        ("oftc.net" "#debian" "#debian-next")
        ("globalgamers" "#Touhou")
        ("twitch.tv" "#tsoding" "#cattzs" "#retched"
         "#bbsssssssss" "#team_treehouse" "#rw_grim")
        ("uworld.se" "#coalgirls"))))

(defun erc-netlist (wi-erc-netlist)
  (dolist (irc-net wi-erc-netlist)
    (append erc-autojoin-channels-alist irc-net)))

(setq erc-autojoin-channels-alist
      (append erc-autojoin-channels-alist wi-erc-netlist-gnome))


;;;
;;; Org
;;;

;; (with-eval-after-load 'org
;;   (setq org-format-latex-options
;;         (plist-put org-format-latex-options :scale 1.5))
;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))

(setq org-email-link-description-format "Email %c: %s") ; More than 30 character

(setq org-capture-templates
        '(("c" "Note" item (file "~/.notes") "%?")

          ;; Requires org-capture-extension
          ;; https://github.com/sprig/org-capture-extension
          ("l" "Protocol" item (file "~/.web.org")
           "[[%:link][%:description]]\n%i"
           :immediate-finish t)
          ("L" "Protocol Link" item (file "~/.web.org")
           "[[%:link][%:description]]"
           :immediate-finish t)

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

          ("i" "TODO" entry (file "/TODO.gpg") "* %?")
          ("n" "pdfview" item (file "~/.pdf-notes") "%a %?")
          ("p" "phrase" item (file "phrase.org") "%?")
          ("e" "emacs" plain (file "emacs.org") "%?")
          ("g" "guix" plain (file "guix.org") "%?")
          ("r" "read" plain (file "read.org") "%?")
          ("o" "misc" plain (file "misc.org") "%?")
          ("m" "music" plain (file "music.org") "%?")
          ("v" "video" plain (file "video.org") "%?")
          ("b" "blog" plain (file "~/public_html/IDEA.org") "%?")
          ("w" "work" plain (file "~/Documents/work.org") "%?")))

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/org/"
         :base-extension "org"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 ; Just the default for this project.
         :auto-preamble t
         :auto-sitemap t
         :sitemap-filename "index")))

(defun wigust-mir-org-uniq ()
  "Remove duplicate subheadings, preserving order."
  ;; See <http://lists.gnu.org/archive/html/emacs-orgmode/2018-01/msg00000.html>.
  (interactive)
  (let ((seen (make-hash-table :test 'equal))
        (removed 0))
    (save-excursion
      (org-map-entries (lambda ()
                         (let ((heading (org-get-heading t t t t)))
                           (if (not (gethash heading seen))
                               (puthash heading t seen)
                             (org-cut-subtree)
                             (org-backward-heading-same-level 1)
                             (setq removed (1+ removed)))))
                       (format "LEVEL=%s" (1+ (org-current-level)))
                       'tree))
    (message "Removed %d duplicates" removed)))

;; See <http://mbork.pl/2017-12-04_Embedding_files_in_Org-mode>.
(defun wi-org-insert-file (filename)
  "Insert Elisp code block recreating file named FILENAME."
  (interactive "f")
  (let ((base64-string
	 (with-temp-buffer
	   (insert-file-contents-literally filename)
	   (base64-encode-region (point-min) (point-max))
	   (buffer-string))))
	(insert (format "#+BEGIN_SRC emacs-lisp :results output silent\n  (with-temp-file %S\n    (insert (base64-decode-string\n      %S)))\n#+END_SRC" filename base64-string))))


;;;
;;; Manage daily TODO files.
;;;

;; Source <http://sachachua.com/blog/2018/01/2018-01-23-emacs-news/>.
;; Origin <https://gist.github.com/prathik/ae2899ae2c432dcb0cfe966aa3683eb3>.

(defun wi-todo-create-directory (directory)
  "Creates the todo directory."
  (if (file-exists-p directory) (message "Director exists")
    (make-directory directory)
    (message "Directory created")))

(defun wi-create-todo-file (directory filename)
  "Checks if the todo file exists if not creates it."
  (wi-todo-create-directory directory)
  (if (file-exists-p filename) (message "Todo exists for the day")
    (write-region "" nil filename)))

(defun wi-open-todo-file (directory)
  "Opens a todo file for the current day."
  (let ((filename (concat directory "/"
                          (format-time-string "%Y-%m-%d") ".org")))
    (wi-create-todo-file directory filename)
    (find-file filename)))

(defun wi-open-todo-file-interactive ()
  "Creates a daily todo file.
  Track what needs to be done for the day.  Plan your day better.
  See what you have accomplished at the end of the day."
  (interactive)
  (wi-open-todo-file "~/org"))

(bind-key "C-c c t" 'wi-open-todo-file-interactive)


;;;
;;; ZNC
;;;

;; https://raw.githubusercontent.com/vincentbernat/dot.emacs/master/znc.conf.el

;;; Code:

(defun vbe:znc-add-server (server port user networks)
  "Add a server to the list of ZNC servers.

We use SSL inconditionaly. Moreover, we don't store the password
but put nil instead. At least, we tweak the username to contain
the network name later, this will be separated again."
  (add-to-list 'znc-servers
               (list server port
                     nil ;; SSL enabled
                     (mapcar (function (lambda (slug)
                                         (list slug
                                               (format "%s/%s" user slug)
                                               nil)))
                             networks))))

(defun vbe:znc-erc-connector (&rest R)
  "Connect to ERC using and retrieve password with `auth-source-search'.

Moreover, handle multiple networks by sending the password with
the appropriate network slug that we extract from the nick."
  (let* ((user (nth 0 (split-string (plist-get R :nick) "/")))
         (slug (nth 1 (split-string (plist-get R :nick) "/")))
         (port (plist-get R :port))
         (found (nth 0 (auth-source-search :host (plist-get R :server)
                                           :port port
                                           :user user
                                           :require '(:user :secret)
                                           :max 1))))
    (if found
        (let ((password (let ((secret (plist-get found :secret)))
                          (if (functionp secret)
                              (funcall secret)
                            secret))))
          (plist-put R :password (format "%s/%s:%s" user slug password))
          (plist-put R :nick user)
          (apply 'erc R)))))

(setq znc-erc-connector 'vbe:znc-erc-connector)

;; ;; Define networks
(use-package znc
  :defer 5
  :config (vbe:znc-add-server "localhost" 8060 "natsu" '(freenode twitch)))

(use-package emms-setup
  :defer 5
  :config
  (emms-standard)
  (emms-default-players)

  (setq emms-player-next-function 'emms-next-noerror)
  (emms-mode-line -1)
  (setq emms-playing-time-display-p nil)

  (use-package emms-player-mpv
    :config
    ;; (add-to-list 'emms-player-mpv-parameters "--volume=40")
    (add-to-list 'emms-player-list 'emms-player-mpv)
    (add-to-list 'emms-player-mpv-parameters "--no-video")
    (add-to-list 'emms-player-mpv-parameters "--no-resume-playback")
    (add-to-list 'emms-player-mpv-parameters "--keep-open=no")
    (add-to-list 'emms-player-mpv-parameters (concat "--audio-device="
                                                     ‎wi-headphones))))

(use-package helm-emms
  :after emms-setup
  :config
  (setq helm-emms-use-track-description-function t)
  (setq emms-track-description-function (lambda (v) (assoc-default 'name v)))
  (setq emms-source-file-default-directory "/srv/music")
  (add-to-list 'helm-emms-music-extensions "mkv")
  (add-to-list 'helm-emms-music-extensions "webm"))

(defun helm-wigust-stream (func)
  ""
  (interactive (list (completing-read "Engine: " '(chromium streamlink))))
  (helm :sources (helm-build-sync-source "urls"
                   :action (lambda (candidate)
                             (funcall (cond ((string-equal func "chromium")
                                             'browse-url-chromium)
                                            ((string-equal func "streamlink")
                                             'browse-url-streamlink))
                                      candidate))
                   :candidates '("https://www.twitch.tv/entr_ru"
                                 "https://www.youtube.com/user/streamguild"
                                 "https://www.twitch.tv/artgameslp"
                                 "https://www.youtube.com/user/ArtGamesLP")
                   :fuzzy-match t)
        :buffer "*helm urls*"))


;;;
;;; Misc
;;;

;; Origin <https://www.emacswiki.org/emacs/RecentFiles>.

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(recentf-mode 1)
(setq recentf-max-menu-items 50)

(setq hl-sexp-background-color "darkseagreen2")

(when (and (require 'edit-server nil t) (daemonp))
  (edit-server-start))

(autoload #'terminal-here-launch-in-directory "terminal-here")
(defun wi-terminal-here-launch (&optional wi-terminal-here-dark)
  "Launch a terminal in the project directory.

With a prefix argument, launch a terminal with a dark theme in
the project directory."
  (interactive)
  (let* ((project (projectile-project-name))
         (project-name (if (string-equal project "-")
                           (read-string "Project: ")
                         project))
         (project-directory (if (string-equal project "-")
                                (read-directory-name "Directory: ")
                              (projectile-project-root)))
         (terminal-here-terminal-command
          `("env" "STY=" ; Make sure screen doesn't complain STY is set.
            "xterm" "-title" ,(concat "xterm-screen-" project-name)
            ,@(if (or wi-terminal-here-dark current-prefix-arg)
                  '("-bg" "black" "-fg" "white")
                '())
            "-e" "screen" "-S" ,project-name)))
    (terminal-here-launch-in-directory project-directory)))

;; See <https://www.emacswiki.org/emacs/DoWhatIMean>
(setq dired-dwim-target t)

(blink-cursor-mode)

(defun wi-manoj-dark ()
  (interactive)
  (load-theme 'manoj-dark)
  (custom-theme-set-faces
   'manoj-dark
   '(which-key-command-description-face ((t (:inherit font-lock-function-name-face :height 1.0))))
   '(fringe ((t (:background "black" :foreground "Wheat"))))
   '(header-line
     ((t (:background "black" :foreground "grey90" :height 0.9))))
   '(scroll-bar ((t (:background "black" :foreground "WhiteSmoke"))))
   ;; '(mode-line ((t (:background "WhiteSmoke" :foreground "black"))))
   ;; '(mode-line-inactive ((t (:background "black" :box nil))))
   '(mode-line-buffer-id ((t (:background "grey15" :foreground "red"))))))

(defun wi-guix-hydra-latest-builds ()
  (interactive)
  (let ((guix-hydra-url "https://berlin.guixsd.org"))
    (guix-hydra-latest-builds 100)))

(default-text-scale-mode)
(global-git-gutter-mode)

(defun wi-copy-file-name ()
  "Return current buffer file name."
  (interactive)
  (kill-new (buffer-file-name)))

(add-hook 'diff-mode-hook (lambda () (setq-local truncate-lines t)))

(defun wi-sort-sexps (reverse beg end)
  "Sort sexps in the Region."
  (interactive "*P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((nextrecfun (lambda () (skip-syntax-forward "-.>")))
          (endrecfun  #'forward-sexp))
      (sort-subr reverse nextrecfun endrecfun))))

(add-hooks
 '(((diff-mode-hook dired-mode-hook proced-post-display-hook
     shell-mode-hook)
    . guix-prettify-mode)
   ((c-mode-hook) . ggtags-mode)
   ((scheme-mode-hook) . guix-devel-mode)
   ((prog-mode-hook) . rainbow-delimiters-mode)
   ((prog-mode-hook
     minibuffer-inactive-mode-hook
     geiser-repl-mode-hook
     git-commit-mode-hook
     org-mode-hook)
    . smartparens-strict-mode)
   ((prog-mode-hook) . yas-minor-mode)
   ((prog-mode-hook) . hs-minor-mode )
   ((erc-mode-hook) . (lambda () (setq truncate-lines t)))))

(defun wi-find-stumpwm-init-file ()
  "Edit the `stumpwm-init-file', in another window."
  (interactive)
  (find-file-other-window (expand-file-name "~/.stumpwm.d/init.lisp")))

;; Deletes up to the provided character
;; Doesn’t delete the provided character
;; Starts the point from before the character rather than after
;;
;; Source: https://www.emacswiki.org/emacs/ZapUpToChar
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; <https://www.reddit.com/r/emacs/comments/7htdzk/show_reddit_prettyprint_debugger_frames/>
(defun wi-debugger-pp-frame ()
  (interactive)
  (let ((inhibit-read-only t)
        (frame (backtrace-frame (debugger-frame-number))))
    (set-buffer (pop-to-buffer "*BT: Frame*"))
    (cl-destructuring-bind (special fn &rest args) frame
      (erase-buffer)
      (progn
        (insert "(" (pp-to-string fn))
        (dolist (arg args)
          (insert "\n" (pp-to-string arg)))
        (insert ")"))
      (goto-char (point-min))
      (indent-pp-sexp))))

;; (define-key debugger-mode-map "r" 'wi-debugger-pp-frame)

(defun wi-god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "grey75"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "grey20" "grey90")))))))

(add-hook 'god-mode-enabled-hook 'wi-god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'wi-god-mode-update-cursor)

(setq mml-secure-insert-signature 'always)

;; TODO: Add to guix emacs package
(setq ispell-aspell-dict-dir "/run/current-system/profile/lib/aspell")

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(TODO\\|FIXME\\):" 1
                                       font-lock-warning-face t)))))

(add-hook 'shell-mode-hook (lambda ()
                             (progn (setq paragraph-separate "[ 	]*$")
                                    (setq paragraph-start "\\|[ 	]*$"))))

(let ((youtube-rss-channel-id
       "https://www.youtube.com/feeds/videos.xml?channel_id=")
      (youtube-rss-user "https://www.youtube.com/feeds/videos.xml?user="))
  (setq elfeed-feeds
        `("http://nullprogram.com/feed/"
          "http://www.scheme.dk/planet/atom.xml"
          "https://lwn.net/headlines/newrss"
          "https://fedoramagazine.org/feed/"
          "http://planet.emacsen.org/atom.xml"
          "http://steckerhalter.tk/index.xml"
          "https://www.reddit.com/r/freegames/.rss"
          "https://www.bennee.com/~alex/blog/feed/"
          ("https://bitlove.org/jupiterbroadcasting/bsdnowhd/feed" video)
          (,(concat youtube-rss-channel-id "UC2eYFnH61tmytImy1mTYvhA")
           video) ; Luke Smith
          (,(concat youtube-rss-channel-id "UCkK9UDm_ZNrq_rIXCz3xCGA")
           video) ; Brian Lunduke
          (,(concat youtube-rss-channel-id "UCMV8p6Lb-bd6UZtTc_QD4zA")
           video) ; Baggers
          (,(concat youtube-rss-channel-id "UCbHXJGd7c8Hy4z0-YX1Jf3Q")
           video) ; Matt Hartley
          (,(concat youtube-rss-user "LDCNow") video)
          (,(concat youtube-rss-user "tuxreviews") video)
          (,(concat youtube-rss-user "EposVox") video)
          (,(concat youtube-rss-user "gotbletu") video)
          (,(concat youtube-rss-user "metalx1000") video))))

(defun wi-fullname-and-email ()
  (format "%s <%s>" user-full-name user-mail-address))

(define-skeleton copyright
  "Insert a copyright by $USER notice at cursor."
  "FULL_NAME <EMAIL>: "
  comment-start
  "; Copyright © " `(format-time-string "%Y") " "
  (or (wi-fullname-and-email) str)
  '(if (copyright-offset-too-large-p)
       (message "Copyright extends beyond `copyright-limit' and won't be updated automatically."))
  comment-end \n)

(setq copyright-names-regexp (wi-fullname-and-email))

;; TODO: Add to guix (add-hook 'before-save-hook 'copyright-update)

(setq quickurl-format-function (lambda (url) (format "<%s>" (quickurl-url-url url))))

;; `w3m' fonts
(setq w3m-fill-column 80)

;; TODO: debpaste API broken
;; (setq debpaste-user-name "wigust")

;; `eww' fonts
(setq shr-width 80)
(setq shr-use-fonts nil)

;; Toggle show-paren-mode on
(show-paren-mode)

;; Don't use ido
(setq projectile-completion-system 'default)

(setq helm-locate-project-list (wi-list-files-in-dir wi-projects-directory))

;; Google translate with translate-shell program
(require 'google-translate-mode)
(with-eval-after-load 'google-translate-mode
  (setq trans-target "ru"))

;; Interested in those timezones
(with-eval-after-load 'time
  (setq display-time-world-time-format "%Z\t%d %B %H:%M")
  (setq display-time-world-list '(("Europe/Moscow"    "Europe/Moscow")
                                  ("Europe/Berlin"    "Europe/Berlin")
                                  ("Europe/London"    "Europe/London")
                                  ("Europe/Istanbul"  "Europe/Istanbul")
                                  ("America/Winnipeg" "America/Winnipeg")
                                  ("America/New_York" "America/New_York")
                                  ("Asia/Tokyo"       "Asia/Tokyo"))))

;; List of Email addresses to send patches for `gitpatch-mail' command
(setq gitpatch-mail-database (list "guix-patches@gnu.org"))

(save-place-mode)            ; Remember position in files
(setq mouse-yank-at-point t) ; Ignore mouse position on paste
(setq vc-follow-symlinks t)  ; Do not ask about following link in Git projects
(setq dired-listing-switches (purecopy "-alh")) ; Prettify dired

;; Toggle prettify symbols mode on
(global-prettify-symbols-mode)

 ; Unprettify symbol after the cursor
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; In addition to browse-url-* functions
(defvar browse-url-streamlink-program "streamlink")
(defvar browse-url-streamlink-arguments '("-p" "mpv"))
(defvar browse-url-streamlink-quality "best")
(defun browse-url-streamlink (url &optional new-window)
  "Ask the mpv video player to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-streamlink-arguments' to mpv."
  (interactive (browse-url-interactive-arg "URL: "))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "streamlink " url)
           nil
           browse-url-streamlink-program
           `(,@browse-url-streamlink-arguments
             ,url
             ,browse-url-streamlink-quality))))

;; In addition to browse-url-* functions
(setq browse-url-mpv-program "mpv")
(setq browse-url-mpv-arguments '("--volume=50"))
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

;; Code snippets framework
(with-eval-after-load 'yasnippet
  (setq yas-snippet-dirs
        (append (wi-expand-file-names
                 '("~/.emacs.d/snippets"
                   "~/src/guix/etc/snippets"
                   "~/.guix-profile/share/emacs/yasnippet-snippets"))
                yas-snippet-dirs))
  (yas-reload-all))

;; Simple Mail Transfer Protocol (SMTP)
(with-eval-after-load 'sendmail
  (setq send-mail-function #'smtpmail-send-it)
  (setq smtpmail-smtp-server "smtp.gmail.com"))

;; Origin <https://github.com/alezost/guix.el/pull/9#issuecomment-340556583>.
(with-eval-after-load 'info
  (info-initialize)
  (setq Info-directory-list
        (append (wi-expand-file-names
                 '("~/src/guix/doc"
                   "~/.guix-profile.d/gdb/share/info"
                   "~/.guix-profile.d/autotools/share/info"))
                Info-directory-list)))
;;
;; Alternative: <https://lists.gnu.org/archive/html/help-guix/2017-03/msg00140.html>,
;; see <.bashrc>.

;; Popup completion framework
(with-eval-after-load 'company
  (setq company-clang-insert-arguments nil)
  (setq company-gtags-insert-arguments nil)
  (setq company-semantic-insert-arguments nil))

;; Structured editing
(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  ;; Origin <https://github.com/Fuco1/smartparens/blob/master/docs/pair-management.rst>.
  (sp-pair "“" "”")
  (sp-pair "‘" "’")
  (sp-local-pair 'text-mode "<" ">"))

;; Undo and redo operations on windows and buffers
(winner-mode 1)
(windmove-default-keybindings)

;; Display key bindings help window (after some delay)
(which-key-mode)

;; Set defaults for debbugs-gnu commands
(with-eval-after-load 'debbugs-gnu
  (setq debbugs-gnu-default-packages (list "guix" "guix-patches")))

(cl-defun wi-debbugs-gnu-list
    (&optional (mail-address user-mail-address)
               (suppress t))
  "List bugs on debbugs.gnu.org from USER-MAIL-ADDRESS.

With SUPPRESS non-nil argument exclude unarchived bugs."
  (interactive)
  (let ((debbugs-gnu-current-query `((submitter . ,mail-address))))
    (debbugs-gnu nil nil nil suppress)))

(setq ewmctrl-wmctrl-path "/run/current-system/profile/bin/wmctrl")

(defun wi-debbugs-gnu-guix ()
  "List Guix bugs on debbugs.gnu.org."
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") '("guix")))

(defun wi-debbugs-gnu-guix-patches ()
  "List Guix patches on debbugs.gnu.org."
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") '("guix-patches")))

(defun wi-set-current-frame-80-40 ()
  "Set current frame to 80 pixels width and 40 pixels height."
  (interactive)
  (set-frame-size (selected-frame) 80 40))

(defun wi-set-current-frame-80-24 ()
  "Set current frame to 80 pixels width and 24 pixels height."
  (interactive)
  (set-frame-size (selected-frame) 80 24))

(defun wi-wget-switch (url)
  "Download a file with wget and open it in buffer"
  (interactive "sDownload URL: ")
  (let ((buffer (generate-new-buffer "*wget*")))
    (with-current-buffer buffer
      (insert (shell-command-to-string (concat "wget" " -q" " -O-" " " url))))
    (switch-to-buffer buffer)))

(defun wi-debbugs-get-url (bug-number)
  "Get a debbugs url according to `BUG-NUMBER'"
  (interactive "sBug number: ")
  (kill-new (concat "https://debbugs.gnu.org/cgi/bugreport.cgi?bug="
                    bug-number)))

(defun wi-copy-cgit-guix-path (path)
  "Copy cgit guix path to kill ring"
  (interactive "sPath: ")
  (kill-new (concat "https://git.savannah.gnu.org/cgit/guix.git/tree/" path)))

(defvar wi-guix-git-directory (expand-file-name "~/src/guix"))
(defun wi-magit-show-commit-guix (commit)
  "Show a Git `commit' from the Guix checkout."
  (interactive "sCommit: ")
  (let ((default-directory wi-guix-git-directory))
    (magit-show-commit commit)))

(defun wi-magit-find-file-guix (commit file)
  "Show a `file' from Git `commit' in the Guix checkout."
  (interactive "sCommit: \nsFile: ")
  (let ((default-directory wi-guix-git-directory))
    (magit-find-file commit file)))

(defun wi-set-guix-directory (directory)
  "Set a `GUIX-DIRECTORY' path."
  (interactive "DDirectory: ")
  (setq guix-directory directory))

;; TODO:
;; Origin <https://lists.gnu.org/archive/html/emacs-devel/2017-12/msg00518.html>.
;; See also <https://github.com/legoscia/messages-are-flowing>.
;; (defun wi-soften-hardlines ()
;;     (interactive)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (mail-text)
;;       (while (search-forward hard-newline nil t)
;;         (replace-match "\n"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(safe-local-variable-values
   '((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (bug-reference-bug-regexp . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t nil)))
 '(magit-diff-added ((t (:foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:foreground "#22aa22"))))
 '(magit-diff-context-highlight ((t (:foreground "grey50"))))
 '(magit-diff-removed ((t (:foreground "#aa2222"))))
 '(magit-diff-removed-highlight ((t (:foreground "#aa2222")))))
