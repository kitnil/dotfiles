;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

;; Tip: "M-x e" on `(emacs-init-time)'.

(setq load-prefer-newer t)

;; Makes unpure packages archives unavailable
(setq package-archives nil)

(setq user-mail-address    "go.wigust@gmail.com")
(setq user-full-name       "Oleg Pykhalov")
(setq default-input-method "russian-computer")

(setq display-time-24hr-format t)
(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)

(setq initial-buffer-choice t)

(setq mail-user-agent 'gnus-user-agent)

(add-to-list 'exec-path (expand-file-name "~/.guix-profile.d/gdb/bin"))


;;;
;;; Enable functions
;;;

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)


;;;
;;; Keybindings
;;;
;;; See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html>

(bind-key "<Scroll_Lock>" #'scroll-lock-mode)
(bind-key "<C-mouse-4>"   #'text-scale-increase)
(bind-key "<C-mouse-5>"   #'text-scale-decrease)

(which-key-add-key-based-replacements "C-c &" "yasnippet")

(bind-key "C-c b" 'ibuffer)
(bind-key "<C-down-mouse-1>" 'mc/toggle-cursor-on-click)

(which-key-add-key-based-replacements "C-c v" "magit")
(bind-keys :prefix "C-c v" :prefix-map wi-version-control-map
           ("l" . magit-list-repositories)
           ("s" . magit-status))

(which-key-add-key-based-replacements "C-c f" "find")
(bind-keys :prefix "C-c f" :prefix-map wi-find-map
           ("e" . guix-edit)
           ("f" . ffap))

(which-key-add-key-based-replacements "C-c t" "toggle")
(bind-keys :prefix "C-c t" :prefix-map wi-toggle-map
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
           ("s" . flyspell-mode)
           ("t" . toggle-truncate-lines)
           ("v" . google-translate-mode)
           ("w" . whitespace-mode)
           ("y" . yas-minor-mode))

(which-key-add-key-based-replacements "C-c r" "rething")
(bind-keys :prefix "C-c r" :prefix-map wi-rething-map
           ("r" . revert-buffer)
           ("l" .  redraw-display))

(which-key-add-key-based-replacements "C-c h" "helm")
(bind-keys :prefix "C-c h" :prefix-map wi-helm-map
           ("b" . helm-buffers-list)
           ("i" . helm-imenu)
           ("m" . helm-make)
           ("r" . helm-bookmarks)
           ("s" . helm-pass)
           ("t" . helm-top)
           ("x" . helm-M-x)
           ("y" . helm-show-kill-ring)
           ("w" . helm-stumpwm-commands))

(which-key-add-key-based-replacements "C-c h p" "helm-projectile")
(which-key-add-key-based-replacements "C-c p x" "projectile-shell")
(which-key-add-key-based-replacements "C-c p s" "projectile-search")
(bind-keys :prefix "C-c h p" :prefix-map wi-helm-projectile-map
           ("p" . helm-projectile)
           ("f" . helm-projectile-find-file-dwim)
           ("b" . helm-projectile-switch-to-buffer))

(which-key-add-key-based-replacements "C-c m" "mail")
(bind-keys :prefix "C-c m" :prefix-map wi-mail-map
           ("b" . wi-send-buffer-as-mail))

(which-key-add-key-based-replacements "C-c m g" "gnus")
(bind-keys :prefix "C-c m g" :prefix-map wi-gnus-map
           ("g" . gnus)
           ("s" . switch-to-gnus))

(which-key-add-key-based-replacements "C-c s" "shell")
(bind-keys :prefix "C-c s" :prefix-map wi-shell-map
           ("s" . shell)
           ("c" . compilation-shell-minor-mode)
           ("e" . eshell)
           ("t" . term))

(which-key-add-key-based-replacements "C-c c" "org")
(bind-keys :prefix "C-c c" :prefix-map wi-org-map
           ("c" . org-capture)
           ("a" . org-agenda)
           ("l" . org-store-link))

(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))

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

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s")

(defengine github-hippie
  "https://github.com/search?ref=simplesearch&q=%s+-language:objectivec+-language:java+-language:javascript+-language:csharp+-language:kotlin+-language:swift+-language:php+-language:vue+-language:autohotkey")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

(defengine guix-devel
  "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s&submit=Search%%21&idxname=guix-devel&max=20&result=normal&sort=score")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine searx
  "http://searx.tk/?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s")

(defengine youtube-latest
  "https://www.youtube.com/results?sp=CAJQFA%%253D%%253D&search_query=%s")

(defengine youtube-rss
  "https://www.youtube.com/feeds/videos.xml?channel_id=%s")


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
  (add-hook 'elisp-mode-hook (lambda ()
                               (set (make-local-variable 'prettify-symbols-alist)
                                    wi-elisp--prettify-symbols-alist)))
  (add-hook 'elisp-mode-hook #'prettify-symbols-mode))


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
  (add-hook 'scheme-mode-hook (lambda ()
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
      ("false" . ?F)
      ("NULL"  . ?N)))

  (add-hook 'c-mode-hook (lambda ()
                           (set (make-local-variable 'prettify-symbols-alist)
                                wi-c--prettify-symbols-alist)))

  (add-hook 'c-mode-hook #'prettify-symbols-mode)
  (add-hook 'c-mode-hook #'ggtags-mode))

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
(setq erc-log-insert-log-on-open t)
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
(setq erc-modules (quote (autojoin button completion fill irccontrols list
                                   log match menu move-to-prompt netsplit
                                   networks noncommands readonly ring
                                   smiley stamp track)))

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
       (("freenode.net" "#icecat" "#emacs" "#grub"
         ;; "#clojure" "##math"
         "##c" "#bash"
         ;; "#fedora" "#fedora-admin" "#fedora-devel"
         ;; "#fedora-noc" "#fedora-meeting" "#fedora-qa"
         "#gnu" "#fsf" "#gnus" "#guile" "#guix" "#stumpwm" "#replicant"
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

(setq org-capture-templates
        '(("c" "Note" item (file "~/.notes")
           "%?")

          ;; Requires org-capture-extension
          ;; https://github.com/sprig/org-capture-extension
          ("L" "Protocol Link" plain (file "~/.web.org")
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

          ("o" "misc" plain (file "misc.org")
           "%?")

          ("m" "music" plain (file "music.org")
           "%?")

          ("v" "video" plain (file "video.org")
           "%?")

          ("b" "blog" plain (file "~/public_html/IDEA.org")
           "%?")

          ("w" "work" plain (file "~/Documents/work.org")
           "%?")))


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

;; Define networks
;; (vbe:znc-add-server "localhost" 8060 "natsu" '(freenode))


;;;
;;; Misc
;;;

(setq elfeed-feeds '("http://nullprogram.com/feed/"
                     "http://planet.emacsen.org/atom.xml"
                     "http://www.scheme.dk/planet/atom.xml"))

(defun wi-fullname-and-email ()
  (format "%s <%s>" user-full-name user-mail-address))

(define-skeleton copyright
  "Insert a copyright by $ORGANIZATION notice at cursor."
  "FULL_NAME <EMAIL>: "
  comment-start
  "Copyright © " `(format-time-string "%Y") " "
  (or (format "%s <%s>" (wi-fullname-and-email))
      str)
  '(if (copyright-offset-too-large-p)
       (message "Copyright extends beyond `copyright-limit' and won't be updated automatically."))
  comment-end \n)

(setq copyright-names-regexp (wi-fullname-and-email))

;; TODO: Add to guix (add-hook 'before-save-hook 'copyright-update)

(setq quickurl-format-function (lambda (url) (format "<%s>" (quickurl-url-url url))))

(setq w3m-fill-column 80)

(setq debpaste-user-name "wigust")

(with-eval-after-load 'eww
  (setq shr-width 80)
  (setq shr-use-fonts nil))

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
        ("." . browse-url-firefox)))

(with-eval-after-load 'sendmail
  (setq send-mail-function #'smtpmail-send-it)
  (setq smtpmail-smtp-server "smtp.gmail.com"))

;; Code from: https://github.com/alezost/guix.el/pull/9#issuecomment-340556583
(with-eval-after-load 'info
  (info-initialize)
  (setq Info-directory-list
        (append (wi-expand-file-names (list "~/src/guix/doc"
					    "~/.guix-profile.d/gdb/share/info"
					    "~/.guix-profile.d/autotools/share/info"))
                Info-directory-list)))
;;
;; Alternative: https://lists.gnu.org/archive/html/help-guix/2017-03/msg00140.html
;; See <~/.bashrc>

(setq yas-snippet-dirs (list "~/.emacs.d/snippets"
                             "~/.guix-profile/share/emacs/yasnippet-snippets/"))

(with-eval-after-load 'company
  (setq company-clang-insert-arguments nil)
  (setq company-gtags-insert-arguments nil)
  (setq company-semantic-insert-arguments nil))

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
