;; Copyright © 2017, 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

;; Tip: "M-x e" on `(emacs-init-time)'.

;; Prettify without breaking indentation,
;; Origin <http://endlessparentheses.com/using-prettify-symbols-in-clojure-and-elisp-without-breaking-indentation.html>.

;; Prevent stale elisp bytecode from shadowing more up-to-date source
;; files.  Origin <https://github.com/technomancy/better-defaults>.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(autoload 'f-directories "f" nil t)
(autoload 'string-trim-right "subr-x" nil t)
(autoload 'flet "cl" nil t)

(setq load-prefer-newer t)

;; (require 'benchmark-init)

;; To disable collection of benchmark data after init is done.
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(setq package-archives nil) ; Makes unpure packages archives unavailable

(setq user-mail-address "go.wigust@gmail.com")
(setq user-full-name "Oleg Pykhalov")
(setq default-input-method "russian-computer") ; <C-\> keyboard layout

(setq display-time-24hr-format t) ; No AM/PM
(setq calendar-date-style 'european) ; day/month/year style calendar
(setq calendar-week-start-day 1) ; Monday is the first day of the week

(setq initial-buffer-choice t) ; Nothing after starting Emacs
(setq initial-scratch-message nil) ; Don't put text in *scratch* buffer

(setq smtpmail-queue-mail t) ; Call after typing M-x `smtpmail-send-queued-mail'

(menu-bar-mode -1)
(set-scroll-bar-mode 'right)
(scroll-bar-mode -1)

(setq ffap-file-finder 'org-open-file)

;; Default from Emacs 26
;; See <http://git.savannah.gnu.org/cgit/emacs.git/commit/etc/NEWS?id=72ee93d68daea00e2ee69417afd4e31b3145a9fa>
(setq print-quoted t)

;; Look for audio devices ‘mpv --audio-device=help’
(defvar ‎wi-headphones "pulse/alsa_output\
.usb-Logitech_Logitech_USB_Headset-00.analog-stereo"
  "My USB headphones")

(autoload 'server-running-p "server")
(unless (server-running-p)
  (server-start)
  )

;; Encrypt Email message with Gnupg
(with-eval-after-load 'message
  (require 'jl-encrypt)
  (add-hook 'message-setup-hook 'mml-secure-encrypt-if-possible)
  (let ((map message-mode-map))
    (define-key map (kbd "C-c s") 'ispell-region)
    (define-key map (kbd "C-c e") 'wi-replace-with-brackets-ellipsis)))

(with-eval-after-load 'prog-mode
  (let ((map prog-mode-map))
    (define-key map (kbd "<f5>") 'recompile)))

(with-eval-after-load 'gnus-art
  (let ((map gnus-article-mode-map))
    (define-key map (kbd "C-c c") 'wi-magit-show-commit-guix)
    (define-key map (kbd "C-c d") 'wi-gnus-browse-debbugs)))

(with-eval-after-load 'dired
  (require 'dired-x))

(with-eval-after-load 'info
  (let ((map Info-mode-map))
    (define-key map (kbd "<f8>") 'wi-info-remote-copy-current-node)))

(defhydra hydra-dabbrev-expand
  (:color red)
  "dabbrev-expand"
  ("/" wi-dabbrev-expand "expand")
  ("u" undo "undo"))

(defhydra hydra-spelling ()
  "
  ^
  ^Spelling^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^───────
  _q_ quit            _<_ previous        _c_ correction
  ^^                  _>_ next            _d_ dictionary
  ^^                  _f_ check           _m_ mode
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("<" flyspell-correct-previous :color pink)
  (">" flyspell-correct-next :color pink)
  ("c" ispell)
  ("d" ispell-change-dictionary)
  ("f" flyspell-buffer)
  ("m" flyspell-mode))

(with-eval-after-load 'vc-git
  (let ((map vc-git-log-edit-mode-map))
    (define-key map (kbd "C-c /") 'hydra-dabbrev-expand/body)
    (define-key map (kbd "C-c l") 'vc-chlog)))

(autoload 'crux-transpose-windows "crux" nil t)
(autoload 'crux-open-with "crux" nil t)

(autoload 'notmuch-search "notmuch" nil t)

(with-eval-after-load 'notmuch
  (setq mail-user-agent 'gnus-user-agent))

(setq mail-user-agent 'gnus-user-agent)

(with-eval-after-load 'mailcap
  (add-to-list 'mailcap-mime-extensions '(".scm" . "text/x-scheme")))

(global-undo-tree-mode)
(projectile-global-mode)

(autoload 'debbugs-browse-url "debbugs-browse" nil t)


;;;
;;; Customize
;;;

(defgroup wi nil
  "Settings for `wi'."
  :prefix "wi-"
  :group  'wi)

(defcustom wi-git "/srv/git"
  "Directory containing Git repositories."
  :type 'directory
  :group 'wi)

(defcustom wi-src (expand-file-name "~/src")
  "Source code directory."
  :type 'directory
  :group 'wi)


;;;
;;; Variables
;;;

(defvar wi-debian-paste-regexp
  (rx-to-string
   `(and "http" (* "s") "://paste.debian.net/" (+ alnum) (* "/")) t)
  "Regexp matching Debian paste URL.")

(defvar wi-url-gnu-lists-regexp
  (rx-to-string
   `(and "http" (* "s") "://lists.gnu.org" (* alnum)) t)
  "Regexp matching GNU mailing lists URL.")

(defun wi-debian-paste-raw (str)
  "Return a raw URL from original."
  (funcall (-lambda ((protocol s domain nth s))
             (mapconcat 'identity
                        (list protocol s domain "plain" nth s)
                        "/"))
           (split-string str "/")))

(defun wi-browse-url-paste-debian (url &optional new-window)
  "Download a snippet from paste.debian.net and open it in a buffer."
  (wi-wget-switch (wi-debian-paste-raw url)))

(defvar wi-lwn-regexp
  (rx-to-string
   `(and "http" (* "s") "://lwn.net/Articles/"
         (+ alnum) (* "/") (* "rss")) t)
  "Regexp matching LWN GNU/Linux news site.")

(defvar wi-url-hydra-regexp
  (rx-to-string
   `(and "http" (* "s") "://hydra.gnu.org" (* "/")) t)
  "Regexp matching GNU Hydra CI.")

(defvar wi-url-gnunet-bot-log-regexp
  (rx "http" (zero-or-one "s") "://gnunet.org/bot/log/"
      (one-or-more alphabetic) (zero-or-one "/") line-end)
  "Regexp matching GNU Hydra CI.")

(defvar wi-twitch-url-regexp
  (rx "http" (zero-or-more "s") "://" (zero-or-more "www.")
      "twitch.tv")
  "Regexp matching Twitch.")

(defvar wi-twitch-video-url-regexp
  (concat wi-twitch-url-regexp
          (rx "/videos/" (one-or-more digit) line-end))
  "Regexp matching Twitch videos web-page.")

(defvar youtube-url-regexp
  (rx "http" (zero-or-more "s") "://" (zero-or-more "www.")
      "youtube.com")
  "Regexp matching YouTube.")

(defvar youtube-url-video-regexp
  (concat youtube-url-regexp
          (rx "/watch?v="
              (one-or-more (or alphanumeric "-" "_"))
              line-end))
  "Regexp matching YouTube videos web-page.")

(defvar youtube-short-url-video-regexp
  (rx "http" (zero-or-more "s") "://" (zero-or-more "www.")
      "youtu.be/" (one-or-more (or alphanumeric "-" "_")) line-end)
  "Regexp matching YouTube short URL.")

(defvar wi-url-github-regexp
  (rx "http" (zero-or-one "s") "://github.com")
  "Regexp matching GitHub.")

(defvar wi-url-melpa-regexp
  (rx "http" (zero-or-one "s") "://melpa.org")
  "Regexp matching Melpa.")

(defun youtube-free-url (url)
  "Convert youtube.com to hooktube.com URL and put into `kill-ring'.

WARNING:  hooktube.com requries non-free JavaScript."
  (interactive
   (let ((clipboard (x-get-clipboard)))
     (list
      (if (string-match-p youtube-url-video-regexp
                          clipboard)
          clipboard
        (read-string "YouTube video URL: ")))))
  (kill-new (concat "https://hooktube.com/watch?v="
                    (car (last (split-string (car (last (split-string url
                                                                      "/")))
                                             "="))))))

(cl-defmacro wi-url-savannah-git-commit-regexp (repository &optional (news))
  `(rx "http" (zero-or-one "s") "://git.savannah.gnu.org/"
       (zero-or-one "c") ,(format "git/%s.git/commit/" repository)
       ,(if news "etc/NEWS" "") "?id="
       (zero-or-more alphanumeric)
       line-end))

(defvar wi-url-emacs-git-commit-regexp
  (wi-url-savannah-git-commit-regexp "emacs" (list :news t)))

(defvar wi-url-guix-git-commit-regexp
  (wi-url-savannah-git-commit-regexp "guix"))

(defmacro define-wi-browse-url-git-commit (repository directory)
  `(progn
     (defun ,(intern (concat "browse-url-" (symbol-name repository) "-git-commit")) (url &optional new-window)
       "Show a Git `commit' from the " ,(symbol-name repository) " checkout.

If no commit hash provides, show a commit from hash at current point."
       (interactive (list (read-string "Commit: " nil nil (word-at-point))))
       (let ((default-directory ,directory)
             (commit (car (last (split-string url "=")))))
         (magit-show-commit commit)))))

(defvar wi-emacs-git-directory (expand-file-name "~/src/emacs")
  "Directory containing Emacs Git repository.")
(define-wi-browse-url-git-commit emacs wi-emacs-git-directory)

(defvar wi-guix-git-directory (expand-file-name "~/src/guix")
  "Directory containing Guix Git repository.")
(define-wi-browse-url-git-commit guix wi-guix-git-directory)

(setq browse-url-browser-function
      `(("^ftp://.*" . browse-ftp-tramp)
        (,(format "^%s\\(%s\\)?\\([[:digit:]]+\\)$"
	  "https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/"
	  (regexp-quote "cgi/bugreport.cgi?bug="))
         . debbugs-browse-url)
        (,youtube-url-video-regexp . browse-url-mpv)
        (,youtube-short-url-video-regexp . browse-url-mpv)
        (,wi-twitch-video-url-regexp . browse-url-mpv)
        (,wi-twitch-url-regexp . browse-url-streamlink)
        (,wi-url-hydra-regexp . browse-url-firefox)
        (,wi-lwn-regexp . eww-browse-url)
        (,wi-url-gnu-lists-regexp . eww-browse-url)
        (,wi-url-gnunet-bot-log-regexp . eww-browse-url)
        (,wi-debian-paste-regexp . wi-browse-url-paste-debian)
        (,wi-url-github-regexp . browse-url-chromium)
        (,wi-url-melpa-regexp . browse-url-chromium)
        (,wi-url-emacs-git-commit-regexp . browse-url-emacs-git-commit)
        (,wi-url-guix-git-commit-regexp . browse-url-guix-git-commit)
        ("." . browse-url-firefox)))

(defcustom ffap-info-finder 'info
  "The command called by `wi-info-at-point' to find an Info file."
  :type 'function
  :group 'ffap
  :risky t)

(defun wi-info-at-point (&optional filename)
  "Start Info, defaulting to file at point.  See `ffap'. "
  (interactive)
  (or filename (setq filename (thing-at-point 'filename t)))
  (cond
   ((and ffap-info-regexp
         (string-match ffap-info-regexp filename))
    (funcall ffap-info-finder filename))
   ((error "No such file or directory `%s'" filename))))

(defun delete-file-at-point (&optional filename)
  "Remove file, defaulting to file at point."
  (interactive)
  (or filename (setq filename (thing-at-point 'filename t)))
  (delete-file filename))

(autoload 'browse-at-remote--remote-ref "browse-at-remote")

(defun wi-github-issue-at-point (&optional issue)
  "Start `browse-url', defaulting to issue at point.  See `ffap'. "
  (interactive)
  (or issue (setq issue (thing-at-point 'number t)))
  (if (numberp issue)
      (browse-url
       (concat (car (browse-at-remote--remote-ref default-directory))
               "/issues/" (number-to-string issue)))
    (error "No issue number at point `%s'" issue)))

(autoload 'fci-mode "fill-column-indicator"
  "Indicate the location of the fill column by drawing a thin
line at fill column." t)

(defun ffap-info-p (filename)
  "If FILENAME is Info page, return it."
  (when (string-match-p (rx-to-string `(and ".info"
                                            (zero-or-more ".gz")
                                            line-end)
                                      t)
                        filename)
    filename))

(defun ffap-man-p (filename)
  "If FILENAME if Man page, return it."
  (when (string-match-p (rx "/man" (zero-or-more digit)
                            "/" (one-or-more (or alphanumeric "." "-" "_"))
                            (zero-or-more ".gz")
                            line-end)
                        filename)
    filename))

(autoload 'guix-ffap-store-path-p "guix-ffap")

(defcustom guix-profile-path-regexp
  (rx-to-string `(and line-start
                      (or "~" ,(getenv "HOME")) "/.guix-profile/"))
  "Regexp matching Guix profile path."
  :type 'regexp
  :group 'guix)

(defun guix-ffap-profile-path-p (filename)
  "If FILENAME matches `guix-profile-path-regexp', return it."
  (when (string-match-p guix-profile-path-regexp filename) filename))

(defun wi-find-file-at-point (&optional filename)
  "Find FILENAME, guessing a default from text around point.
If `ffap-url-regexp' is not nil, the FILENAME may also be an URL.
With a prefix, this command behaves exactly like `ffap-file-finder'.
If `ffap-require-prefix' is set, the prefix meaning is reversed.
See also the variables `ffap-dired-wildcards', `ffap-newfile-prompt',
and the functions `ffap-file-at-point' and `ffap-url-at-point'."
  (interactive)
  (if (and (called-interactively-p 'interactive)
	   (if ffap-require-prefix (not current-prefix-arg)
	     current-prefix-arg))
      ;; Do exactly the ffap-file-finder command, even the prompting:
      (let (current-prefix-arg)		; we already interpreted it
	(call-interactively ffap-file-finder))
    (or filename (setq filename (ffap-prompter)))
    (let ((url (ffap-url-p filename))
          (info-page (ffap-info-p filename))
          ;; (guix-profile-dir (guix-ffap-profile-path-p filename))
          (man-page (ffap-man-p filename)))
      (cond
       (url
	(let (current-prefix-arg)
	  (funcall ffap-url-fetcher url)))
       (info-page
        (let (current-prefix-arg)
          (info info-page)))
       (man-page
        (let (current-prefix-arg)
          (man man-page)))
       ;; (guix-profile-dir
       ;;  (let (current-prefix-arg)
       ;;    (guix-run-in-shell (concat "readlink " filename))))
       ((and ffap-pass-wildcards-to-dired
	     ffap-dired-wildcards
	     (string-match ffap-dired-wildcards filename))
	(funcall ffap-directory-finder filename))
       ((and ffap-dired-wildcards
	     (string-match ffap-dired-wildcards filename)
	     find-file-wildcards
	     ;; Check if it's find-file that supports wildcards arg
	     (memq ffap-file-finder '(find-file find-alternate-file)))
	(funcall ffap-file-finder (expand-file-name filename) t))
       ((or (not ffap-newfile-prompt)
	    (file-exists-p filename)
	    (y-or-n-p "File does not exist, create buffer? "))
	(funcall ffap-file-finder
		 ;; expand-file-name fixes "~/~/.emacs" bug sent by CHUCKR.
		 (expand-file-name filename)))
       ;; User does not want to find a non-existent file:
       ((signal 'file-error (list "Opening file buffer"
				  "No such file or directory"
				  filename)))))))

(with-eval-after-load 'ffap
  (add-to-list 'ffap-alist '("\\.patch" . guix-devel-ffap-patch)))

(advice-add 'find-file-at-point :override #'wi-find-file-at-point)

;; Enable functions
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)


;;;
;;; Keybindings
;;;
;;; See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html>.
;;; Watch about hydra <https://www.youtube.com/watch?v=_qZliI1BKzI>.
(defmacro wi-define-keys (prefix prefix-map &rest args)
  "Define keys.
PREFIX - prefix key for these bindings.
PREFIX-MAP - prefix key for these bindings.

Sets the following basend on PREFIX-MAP:
- which - description of these bindings.
- hydra - hydra function name.
- hydra-comment - hydra description of these bindings."
  `(progn
     (bind-keys :prefix ,prefix
                :prefix-map ,(intern (concat "wi-"
                                             (symbol-name prefix-map)
                                             "-map"))
                ,@(mapcar (lambda (arg)
                            (let ((key (car arg))
                                  (func (cadr arg)))
                              (cons key func)))
                          args)
                ("h" . ,(intern (concat "hydra-"
                                        (symbol-name prefix-map)
                                        "/body"))))
     (which-key-add-key-based-replacements ,prefix
       ,(symbol-name prefix-map))
     (which-key-add-key-based-replacements ,(concat prefix " h")
       "hydra")
     (defhydra ,(intern (concat "hydra-" (symbol-name prefix-map)))
       (:color pink)
       ,(mapconcat 'identity
                   (split-string (symbol-name prefix-map) "-")
                   " ")
       ,@args
       ("q" nil "quit"))))

(defmacro wi-define-switch-to-buffer (name buffer)
  `(defun ,(intern (concat "wi-switch-to-buffer-" (symbol-name name)))
       nil
     (interactive)
     (switch-to-buffer ,buffer)))

(wi-define-switch-to-buffer guile-repl "* Guile REPL *")
(wi-define-switch-to-buffer eww "*eww*")
(wi-define-switch-to-buffer nekrovim "#nekrovim")

(defmacro wi-define-find-file (name file)
  `(defun ,(intern (concat "wi-find-file-" (symbol-name name)))
       nil
     (interactive)
     (find-file ,file)))

(wi-define-find-file guixsd (expand-file-name
                             "~/dotfiles/fiore/magnolia.scm"))

(wi-define-find-file emacs (expand-file-name
                            "~/dotfiles/fiore/natsu/.emacs"))

(defmacro wi-define-magit-status-repo (name directory)
  `(defun ,(intern (concat "wi-magit-status-repo-"
                           (symbol-name name)))
       nil
     (interactive)
     (magit-status ,directory)))

(wi-define-magit-status-repo guix (expand-file-name "~/src/guix"))

(bind-key "<Scroll_Lock>" #'scroll-lock-mode)
(bind-key "<C-mouse-4>" #'text-scale-increase)
(bind-key "<C-mouse-5>" #'text-scale-decrease)

(which-key-add-key-based-replacements "C-c &" "yasnippet")

(wi-define-keys "C-c g b" buffer
                ("b" scratch "scratch" :color blue)
                ("f" transpose-frame "tr frame")
                ("i" ibuffer "ibuffer")
                ("l" redraw-display "redraw")
                ("n" next-buffer "next")
                ("p" previous-buffer "previous")
                ("r" revert-buffer "revert")
                ("w" crux-transpose-windows "tr window"))

(wi-define-keys "C-c g b s" buffer-switch
                ("e" wi-switch-to-scratch-elisp "elisp")
                ("g" wi-switch-to-buffer-guile-repl "guile")
                ("w" wi-switch-to-buffer-eww "eww"))

(wi-define-keys "C-c g b s i" buffer-switch-irc
                ("n" wi-switch-to-buffer-nekrovim "nekrovim")
                ("SPC" erc-track-switch-buffer "track"))

(wi-define-keys "C-c g b f" buffer-file
                ("d" wi-find-file-guixsd "guixsd")
                ("e" wi-find-file-emacs "emacs"))

(wi-define-keys "C-c g b m" move-buffer
                ("b" buf-move-left "left")
                ("f" buf-move-right "right")
                ("n" buf-move-down "down")
                ("p" buf-move-up "up"))

(bind-key "<C-down-mouse-1>" 'mc/toggle-cursor-on-click)

(wi-define-keys "C-c g a" text
                ("/" wi-dabbrev-expand "expand")
                ("a" align-regexp "align rx")
                ("P" wi-mark-paragraph+sort-lines "paragraph")
                ("u" undo "undo")
                ("p" move-text-up "up")
                ("n" move-text-down "down"))

(wi-define-keys "C-c g a e" expand
                ("'" er/mark-inside-quotes "in quotes")
                ("P" er/mark-inside-pairs "in pairs")
                ("S" er/mark-symbol-with-prefix "prefix symbol")
                ("\"" er/mark-outside-quotes "out quotes")
                ("c" er/mark-comment "comment")
                ("d" er/mark-defun "defun")
                ("e" er/expand-region "region")
                ("E" er/mark-email "email")
                ("m" er/mark-method-call "method")
                ("n" er/mark-next-accessor "next accessor")
                ("p" er/mark-outside-pairs "out pairs")
                ("s" er/mark-symbol "symbol")
                ("u" er/mark-url "url")
                ("w" er/mark-word "word"))

(wi-define-keys "C-c g a m" multicursor
                ("t" mc/mark-next-like-this "next like this")
                ("T" mc/mark-previous-like-this "previous like this")
                ("s" mc/mark-next-symbol-like-this "next symbol")
                ("S" mc/mark-previous-symbol-like-this "previous symbol")
                ("l" mc/mark-next-lines "next line")
                ("L" mc/mark-previous-lines "previous line"))

(wi-define-keys "C-c g a T" text-todo
                ("n" hl-todo-next "next")
                ("o" hl-todo-occur "occur")
                ("p" hl-todo-previous "prev"))

(wi-define-keys "C-c g a P" text-page
                ("[" backward-paragraph "prev paragraph")
                ("]" forward-paragraph "next paragraph")
                ("n" forward-page "next page")
                ("p" backward-page "prev page"))

(wi-define-keys "C-c g a S" text-symbol
                ("n" highlight-symbol-next "next")
                ("p" highlight-symbol-prev "prev"))

(wi-define-keys "C-c g a s" text-sexp
                ("j" sp-join-sexp "join")
                ("s" sp-split-sexp "split")
                ("S" wi-sort-sexps "sort")
                ("u" undo "undo"))

(wi-define-keys "C-c g w" word
                ("t" show-translation "translate"))

(which-key-add-key-based-replacements "C-c g v" "vc")

(wi-define-keys "C-c g v u" merge-upstream
                ("C" guix-mu-copy-and-commit "copy and commit")
                ("N" guix-mu-other-next "other next")
                ("P" guix-mu-other-prev "other prev")
                ("Y" guix-mu-copy "copy")
                ("c" guix-mu-commit "commit")
                ("h" guix-mu-hide-all "hide")
                ("k" guix-mu-kill-sexp "kill-sexp")
                ("n" guix-mu-next "next")
                ("o" guix-mu-other "other")
                ("p" guix-mu-prev "prev")
                ("r" guix-mu-revert "revert")
                ("s" hs-show-all "show")
                ("u" undo "undo")
                ("y" guix-mu-yank "yank"))

(wi-define-keys "C-c g v m" magit
                ("c" magit-commit "commit")
                ("l" magit-list-repositories "repo list" :color blue)
                ("r" magit-diff-toggle-refine-hunk "tg refine")
                ("s" unpackaged/magit-status "status" :color blue))

(wi-define-keys "C-c g v m r" magit-repo
                ("g" wi-magit-status-repo-guix "guix" :color blue))

(wi-define-keys "C-c g v b" browse-at-remote
                ;; TODO: ("g" . wi-browse-at-remote-gnu)
                ("b" browse-at-remote "browse" :color blue))

(wi-define-keys "C-c g v v" vc-hunk
                ("c" magit-commit "commit" :color blue)
                ("e" magit-commit-extend "extend" :color blue)
                ("n" git-gutter:next-hunk "next")
                ("l" git-messenger:popup-message "line")
                ("p" git-gutter:previous-hunk "previous")
                ("r" git-gutter:revert-hunk "revert")
                ("s" git-gutter:stage-hunk "stage"))

(wi-define-keys "C-c g f" open
                ("e" guix-edit "guix package" :color blue)
                ("f" ffap "thing at point" :color blue)
                ("l" recentf-open-files "recent" :color blue)
                ("r" ffap-read-only "RO thing at point" :color blue))

(wi-define-keys "C-c g f d" dumb-jump
                ("g" dumb-jump-go "go")
                ("o" dumb-jump-go-other-window "other window")
                ("e" dumb-jump-go-prefer-external "go external")
                ("x" dumb-jump-go-prefer-external-other-window
                 "go external other window")
                ("i" dumb-jump-go-prompt "prompt")
                ("l" dumb-jump-quick-look "quick look")
                ("b" dumb-jump-back "back"))

(wi-define-keys "C-c g f v" find-vc
                ("g" wi-github-issue-at-point "gh is")
                ("n" next-line "next")
                ("p" previous-line "previous"))

(wi-define-keys "C-c g f b" browse
                ("c" browse-url-conkeror "conkeror" :color blue)
                ("e" eww "eww" :color blue)
                ("g" browse-url-chromium "chromium" :color blue)
                ("i" browse-url-firefox "firefox" :color blue)
                ("m" browse-url-mpv "mpv" :color blue))

(wi-define-keys "C-c g t" toggle
                ("A" auto-save-mode "autosave")
                ("F" flymake-mode "flymake")
                ("P" projectile-global-mode "projectile")
                ("W" fci-mode "fci")
                ("a" abbrev-mode "abbrev")
                ("b" guix-build-log-minor-mode "guix")
                ("c" company-mode "company")
                ("e" prettify-symbols-mode "prettify")
                ("g" guix-prettify-mode "guix")
                ("i" aggressive-indent-mode "aggressive")
                ("l" hl-line-mode "line")
                ("p" smartparens-strict-mode "smartparens")
                ("s" flyspell-mode "flyspell")
                ("t" toggle-truncate-lines "truncate")
                ("v" google-translate-mode "google")
                ("w" whitespace-mode "whitespace")
                ("y" yas-minor-mode "yasnippet"))

(wi-define-keys "C-c g t f" flycheck
                ("f" flycheck-mode "toggle")
                ("l" flycheck-list-errors "list")
                ("n" flycheck-next-error "next")
                ("p" flycheck-previous-error "prev"))

(wi-define-keys "C-c g t c" toggle-highlight
                ("S" highlight-sexp-mode "sexp")
                ("b" rainbow-blocks-mode "blocks")
                ("c" rainbow-mode "colors")
                ("d" rainbow-delimiters-mode "delimiters")
                ("i" rainbow-identifiers-mode "identifiers")
                ("q" highlight-stages-mode "stages")
                ("s" highlight-symbol-mode "symbol"))

(wi-define-keys "C-c g h" helm
                ("&" helm-yas-complete "yasnippet" :color blue)
                ("R" helm-bookmarks "bookmarks" :color blue)
                ("a" helm-world-time "time" :color blue)
                ("b" helm-buffers-list "buffers" :color blue)
                ("f" helm-for-files "files" :color blue)
                ("i" helm-imenu "imenu" :color blue)
                ("j" helm-register "register" :color blue)
                ("l" helm-recentf "recent" :color blue)
                ("m" helm-make "make" :color blue)
                ("r" helm-resume "resume" :color blue)
                ("s" helm-pass "pass" :color blue)
                ("t" helm-top "top" :color blue)
                ("v" wi-helm-wigust-stream "stream" :color blue)
                ("w" helm-stumpwm-commands "stumpwm" :color blue)
                ("x" helm-M-x "M-x" :color blue)
                ("y" helm-show-kill-ring "kill ring" :color blue))

(wi-define-keys "C-c g h H" helm-help
                ("m" helm-man-woman "man" :color blue)
                ("i" helm-info "info" :color blue))

(wi-define-keys "C-c g i" ivy
                ("b" ivy-switch-buffer "switch buffer" :color blue)
                ("f" counsel-find-file "find file" :color blue)
                ("g" counsel-rg "grep" :color blue)
                ("l" ivy-recentf "recent" :color blue)
                ("r" ivy-resume "resume" :color blue)
                ("s" swiper "swiper" :color blue)
                ("x" counsel-M-x "M-x" :color blue))

(wi-define-keys "C-c g i g" counsel-git
                ("f" counsel-git "git" :color blue)
                ("v" counsel-git-grep "grep" :color blue))

(wi-define-keys "C-c g i h" counsel-help
                ("f" counsel-describe-function "function" :color blue)
                ("i" counsel-info-lookup-symbol "symbol" :color blue)
                ("l" counsel-find-library "library" :color blue)
                ("u" counsel-unicode-char "char" :color blue)
                ("v" counsel-describe-variable "variable" :color blue))

(which-key-add-key-based-replacements "C-c g p x" "projectile-shell")
(which-key-add-key-based-replacements "C-c g p s" "projectile-search")
(wi-define-keys "C-c g h p" helm-projectile
                ("b" helm-projectile-switch-to-buffer :color blue)
                ("f" helm-projectile-find-file-dwim :color blue)
                ("p" helm-projectile :color blue))

(wi-define-keys "C-c g m" mail
                ("b" wi-send-buffer-as-mail :color blue))

(wi-define-keys "C-c g m i" irc
                ("s" erc-track-switch-buffer "switch"))

(wi-define-keys "C-c g e" emms
                ("*" pulseaudio-control-set-volume "set vol")
                ("+" emms-volume-raise "inc vol")
                ("-" emms-volume-lower "dec vol")
                ("C-s" helm-emms "emms" :color blue)
                ("P" emms-pause "pause")
                ("d" emms-play-directory "directory")
                ("e" emms "emms" :color blue)
                ("n" emms-next-show "next")
                ("p" emms-previous-show "previous")
                ("r" emms-random-show "random")
                ("s" emms-stop "stop"))

(wi-define-keys "C-c g m d" debbugs
                ("b" debbugs-gnu-bugs "bugs" :color blue)
                ("l" debbugs-gnu "gnu" :color blue)
                ("m" wi-debbugs-gnu-list "wigust" :color blue)
                ("p" debbugs-gnu-patches "patches" :color blue)
                ("s" debbugs-gnu-search "search" :color blue))

(wi-define-keys "C-c g m r" elfeed
                ("r" elfeed "elfeed" :color blue)
                ("g" elfeed-update "update"))

(wi-define-keys "C-c g m g" gnus
                ("=" gnus-summary-expand-window "expand")
                ("RET" gnus-summary-scroll-up "prev page")
                ("S-SPC" gnus-summary-prev-page "next page")
                ("SPC" gnus-summary-next-page "scroll")
                ("d" wi-gnus-browse-debbugs :color :blue)
                ("g" gnus "gnus" :color blue)
                ("G" mbsync "mbsync" :color blue)
                ("k" gnus-summary-kill-thread "kill thread")
                ("n" gnus-summary-next-article "next article")
                ("p" gnus-summary-prev-article "prev article")
                ("o" gnus-summary-top-thread "top thread")
                ("s" switch-to-gnus "switch" :color blue))

(wi-define-keys "C-c g m g m" gnus-message
                (";" wi-replace-with-brackets-ellipsis "ellipsis"))

(wi-define-keys "C-c g m X" redshift
                ("g" redshift-set-temp "set")
                ("n" redshift-decrease-temp "decrease")
                ("p" redshift-increase-temp "increase"))

(wi-define-keys "C-c g s" shell
                ("e" eshell "eshell" :color blue)
                ("t" term "ansi" :color blue)
                ("x" terminal-here-project-launch-multiplexer "xterm" :color blue))

(wi-define-keys "C-c g s s" shell-dumb
                ("M-r" helm-shell-history "history" :color blue)
                ("C" compilation-shell-minor-mode "complition" :color blue)
                ("c" wi-shell-cd-current-dir "cd" :color blue)
                ("s" shell "shell" :color blue))

(wi-define-keys "C-c g c" org
                ("C" open-todo-file-open "new todo file")
                ("a" org-agenda "agenda" :color blue)
                ("c" org-capture "capture" :color blue)
                ("l" org-store-link "link"))

(which-key-add-key-based-replacements "C-c g k" "engine")

;; (bind-key "<f5>" #'aya-create)
;; (bind-key "<f6>" #'aya-expand)
;; (bind-key "<f7>" #'mc/mark-next-like-this)
;; (bind-key "<f8>" #'er/expand-region)
;; (bind-key "<M-f6>" #'god-mode-all)
(bind-key "M-z" #'zap-up-to-char)
(bind-key "C-c g u" #'undo-tree-visualize)
(which-key-add-key-based-replacements "C-c g u" "undo")
(bind-key "C-c g o" #'ace-window)
(which-key-add-key-based-replacements "C-c g o" "ace-window")

; TODO: (bind-key "<C-tab>" #'hs-toggle-hiding scheme-mode-map)

(mapc (lambda (lst)
        (add-to-list 'auto-mode-alist lst))
      '(("PKGBUILD ." shell-script-mode)
        ("\\.conkerorrc" . js-mode)
        ("\\.guile" . scheme-mode)
        ("\\.mbsyncrc" . conf-mode)
        ("manifest" . scheme-mode)))

(mapc (-lambda ((hook key proc))
        (add-hook hook `(lambda ()
                          (local-set-key (kbd ,key) ',proc))))
      '((scheme-mode-hook "<C-return>" eir-eval-in-geiser)
        (sh-mode-hook "C-<return>" eir-eval-in-shell)
        (lisp-mode-hook "C-<return>" eir-eval-in-slime)
        (python-mode-hook "C-<return>" eir-eval-in-python)
        (emacs-lisp-mode-hook "C-<return>" eir-eval-in-ielm)))

(with-eval-after-load 'guix-ui-package
  (let ((map guix-output-list-mode-map))
    (define-key map (kbd "<f6>") 'hl-line-mode)))

(with-eval-after-load 'dired
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c x") 'crux-open-with)))


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
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "h")

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

(defengine google-instant
  "https://www.google.com/webhp?#q=%s&btnI=I")

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
  "https://hydra.gnu.org/search?query=%s"
  :keybinding "c")

(defengine guix-hydra-job
  ;; e.g. gource-0.47
  "https://hydra.gnu.org/job/gnu/master/%s")

(defengine nixos-hydra
  "https://hydra.nixos.org/search?query=%s")

(defengine nixos-hydra-job
  "https://hydra.nixos.org/job/gnu/master/%s.x86_64-linux")

(defmacro wi-defengine-ml-gnu (idxname &optional message-id)
  `(defengine ,(if message-id
                   (intern (concat (symbol-name idxname) "-message-id"))
                 idxname)
     (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
             (if ,message-id "" "%s")
             "&submit=Search%%21"
             (if ,message-id "%%2Bmessage-id%%3A%s" "")
             "&idxname=" ,(symbol-name idxname)
             "&max=20"
             "&result=normal"
             "&sort=score")))

(defengine listinfo-gnu "https://lists.gnu.org/mailman/listinfo/%s")

(wi-defengine-ml-gnu info-gnus-english)
(wi-defengine-ml-gnu emacs-devel t)
(wi-defengine-ml-gnu emacs-devel)
(wi-defengine-ml-gnu emacs-orgmode t)
(wi-defengine-ml-gnu emacs-orgmode)
(wi-defengine-ml-gnu guix-devel t)
(wi-defengine-ml-gnu guix-devel)
(wi-defengine-ml-gnu guix-help t)
(wi-defengine-ml-gnu guix-help)
(wi-defengine-ml-gnu help-gnu-emacs t)
(wi-defengine-ml-gnu help-gnu-emacs)
(wi-defengine-ml-gnu info-gnus-english-message-id)

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
          "&idxname=bug-guix"
          "&idxname=guix-patches"
          "&idxname=guix-devel"
          "&idxname=help-guix"
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
          "&sort=date%%3Alate")
    :keybinding "g")

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
  "https://www.youtube.com/results?aq=f&oq=&search_query=%s")

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

;; Origin <https://emacs.stackexchange.com/a/2473>.
(defun wi-dabbrev-expand ()
  "Insert space and call `dabbrev-expand'."
  (interactive)
  (execute-kbd-macro (kbd "SPC"))
  (call-interactively #'dabbrev-expand))

(defun wi-dabbrev-expand-until-period ()
  "Call `wi-dabbrev-expand' until period before cursor."
  (interactive)
  (unless (string-equal (char-to-string (char-before)) ".")
    (wi-dabbrev-expand)
    (wi-dabbrev-expand-until-period)))

(defun wi-buffer-major-mode (buffer)
  "Return major-mode of BUFFER."
  (cdr (assoc 'major-mode (buffer-local-variables buffer))))

(defun wi-buffers-similar-major-mode ()
  "Return buffer with similar major-mode as in current buffer."
  (-filter (lambda (buffer)
             (string-equal (wi-buffer-major-mode (current-buffer))
                           (wi-buffer-major-mode buffer)))
           (buffer-list)))

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
             (defun ,(intern (concat "wi-insert-" (symbol-name name)))
                 nil
               (interactive)
               (insert text))))
         ,name-text-list))

(defun wi-guix-download (url)
  (interactive "sDownload URL: ")
  (insert
   (shell-command-to-string
    (concat "guix download " url
            " 2>/dev/null" "| tail -n 1" "| tr -d '\n'"))))

(defun wi-switch-to-scratch-elisp ()
  (interactive)
  (switch-to-buffer "*scratch*"))

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

;; Origin <http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/>.
(defun wi-magit-kill-all-buffers ()
  "Kill all Magit buffers."
  (mapc #'kill-buffer (magit-mode-get-buffers)))


;;;
;;; General functions for use
;;;

(defun wi-expand-file-names (files)
    (mapcar (lambda (file) (expand-file-name file)) files))

;; See <https://lists.gnu.org/archive/html/emacs-devel/2017-12/msg00017.html>.
(defun wi-git-log (&optional repo commit)
  "Check REPO for COMMIT and if it exists, display its commit message.
Interactively, prompt for REPO, defaulting to emacs-master, and
for COMMIT, defaulting to the commit hash at point."
  (interactive "p")
  (let* ((git-dir (if repo
		      (read-directory-name
                       "Repo: " "/mnt/data/steve/git/"
                       nil t "emacs-master")
		    "/mnt/data/steve/git/emacs-master"))
	 (commit0
          (or commit
              (read-string "Commit: " nil nil (word-at-point))))
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

(defvar wi-elisp--prettify-symbols-alist
  '(("lambda" . ?λ)
    ("lambda*" . (?λ (Br . Bl) ?*)))
  "Alist of symbol prettifications for `emacs-lisp-mode'.")

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'prettify-symbols-alist)
                 wi-elisp--prettify-symbols-alist)))

;;;
;;; Guile
;;;

(defun projectile-run-guile (&optional pure)
  "Invoke ‘run-guile’ in the project’s root.

With a prefix argument, clean `geiser-guile-load-path'."
  (interactive)
  (let* ((geiser-guile-load-path (if (or pure current-prefix-arg) nil
                                   geiser-guile-load-path))
         (geiser-guile-load-path
          (append (list (expand-file-name (projectile-project-root)))
                  geiser-guile-load-path)))
    (run-guile)))

(defun run-guile-with-directory (directory)
  "Invoke `run-guile' and add directory to %load-path."
  (interactive "DAdd to %%load-path: ")
  (let ((geiser-guile-load-path (append (list directory)
                                        geiser-guile-load-path)))
    (run-guile)))

(defun emacs-add-to-load-path (directory)
  "Add DIRECTORY to `load-path'."
  (interactive "DAdd to `load-path': ")
  (add-to-list 'load-path (expand-file-name directory)))

(with-eval-after-load 'geiser
  (setq geiser-active-implementations '(guile))
  (setq geiser-default-implementation 'guile))

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "/home/natsu/src/guix")
  (setq geiser-guile-binary '("guile" "--no-auto-compile"))

  ;; Origin <https://gnunet.org/bot/log/guile/2018-02-24>
  ;; (setq geiser-guile-load-path (f-entries "~/src"))
  )

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

(autoload 'scheme-smart-complete "scheme-complete" nil t)
;; (eval-after-load 'scheme
;;    '(define-key scheme-mode-map "\e\t" 'scheme-smart-complete))
(setq scheme-default-implementation 'guile)
(setq *scheme-use-r7rs* nil)

(defconst wi-hy--prettify-symbols-alist
  '(("fn" . ?λ)
    ("True" . (?# (Br . Bl) ?t))
    ("False" . (?# (Br . Bl) ?f))))

(add-hooks
 '(((hy-mode-hook)
    . (lambda ()
        (set (make-local-variable 'prettify-symbols-alist)
             wi-hy--prettify-symbols-alist)))))


;;;
;;; Guix
;;;

(setq guix-find-file-function #'org-open-file)

(defcustom guix-hydra-script "~/src/hello-guile/package.scm"
  "Script to get package names appropriate for Guix Hydra."
  :group 'guix-hydra)

(defun guix-hydra-packages (packages)
  "Return a list of packages appropriate for Guix Hydra."
  (remove ""
          (split-string (shell-command-to-string
                         (mapconcat 'identity
                                    (append (list (expand-file-name guix-hydra-script))
                                            packages)
                                    " "))
                        "\n")))

(defun guix-hydra-packages-browse (packages)
  (interactive "sPackages (space separated): ")
  "Open a WEB browser at Guix Hydra for PACKAGES."
  (mapc (lambda (package)
          (engine/search-guix-hydra-job package))
        (guix-hydra-packages (split-string packages " "))))

(setq guix-read-package-name-function
      #'guix-read-package-name-at-point)

(defun wi-guix-hydra-latest-builds (number)
  (interactive
   (list (read-number "Number of latest builds: " 64)))
  (flet ((guix-hydra-latest-builds-custom ()
          (guix-hydra-latest-builds number :project 'gnu :system "x86_64-linux")))
    (if current-prefix-arg
        (let ((guix-hydra-url "https://berlin.guixsd.org"))
          (funcall #'guix-hydra-latest-builds-custom))
      (funcall #'guix-hydra-latest-builds-custom))))

(defcustom guix-substitute-servers
  '("https://berlin.guixsd.org/" "https://hydra.gnu.org/")
  "List of Guix substitute servers."
  :type '(repeat string)
  :group 'guix)

(defun guix-substitute-servers-narinfo (hash)
  (mapcar (lambda (server)
            (mapconcat 'identity
                       (list "wget" "-q" "-O" "-"
                             (concat server hash ".narinfo"))
                       " "))
          guix-substitute-servers))

(with-eval-after-load 'build-farm-url
  (add-to-list 'build-farm-url-alist
               '("http://cuirass.tld" . cuirass)))

(defvar bui-rgrep-directory
  "~/dotfiles/fiore/manifests/"
  "Directory to search for a package in `bui-rgrep-manifests' procedure.")

(defun bui-rgrep-manifests ()
  (interactive)
  (rgrep (substring-no-properties (aref (tabulated-list-get-entry) 0))
         "*.scm"
         (expand-file-name bui-rgrep-directory)))

(with-eval-after-load 'build-farm
  (defun wi-build-farm (job)
    "Wrapper for `build-farm' procedure.

Produces URL as https://ci.guix.info/api/latestbuilds?nr=10&jobset=guix-master&job=opam-2.0.1&system=x86_64-linux"
    (interactive (list (guix-read-package-name)))
    (let ((build-farm-url "https://ci.guix.info")
          (number 10)
          (job (string-trim-right (shell-command-to-string (format "guix-search %s 2>/dev/null" job)))))
      (apply #'build-farm-get-display
             build-farm-url 'build 'latest number (list :project nil
                                                        :jobset (if current-prefix-arg nil "guix-master")
                                                        :job (concat job ".x86_64-linux")
                                                        :system nil)))))


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
  (setq-default c-cleanup-list '(space-before-funcall scope-operator)))

(with-eval-after-load 'semantic
  (global-semantic-decoration-mode t))


;;;
;;; Git Gutter
;;;

(defun wi-git-gutter:stage-hunk ()
  "Stage this hunk like 'git add -p'."
  (interactive)
  (flet ((yes-or-no-p (action)
                      (y-or-n-p
                       (format "%s current hunk ? " action))))
    (git-gutter:query-action "Stage"
                             #'git-gutter:do-stage-hunk
                             #'git-gutter)))

(advice-add 'git-gutter:stage-hunk
            :override #'wi-git-gutter:stage-hunk)

(defun wi-git-gutter-refresh-visible-buffers ()
  "Refresh git-gutter-mode on all visible git-gutter-mode buffers."
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when (and git-gutter-mode (get-buffer-window buff))
        (git-gutter-mode t)))))


;;;
;;; Version control
;;;

(defun vc-chlog ()
  "Insert output of vc-chlog."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (insert (shell-command-to-string (mapconcat 'identity
                                                (list "vc-chlog"
                                                      "| sed 's/^[ \t]*//'"
                                                      "| tail +2")
                                                " ")))))

(defvar magit-read-reuse-message-target "ORIG_HEAD")
(setq magit-read-reuse-message-target "HEAD")

(defun wi-magit-read-reuse-message (prompt &optional default)
  (magit-completing-read prompt (magit-list-refnames)
                         nil nil nil 'magit-revision-history
                         (or default
                             (and (magit-rev-verify
                                   magit-read-reuse-message-target)
                                  magit-read-reuse-message-target))))

(advice-add 'magit-read-reuse-message
            :override #'wi-magit-read-reuse-message)

;; TODO: This is slow down Emacs startup.
;; (magit-org-todos-autoinsert)

(setq magit-repository-directories (f-directories wi-src))
(setq magit-repository-directories-depth 1)
(setq magit-log-arguments '("--graph" "--color" "--decorate" "-n64"))
(setq magit-log-section-arguments (list "-n256" "--decorate"))

;; Use `magit-describe-section'
(defun wi-local-magit-initially-hide-unmerged (section)
  (and (not magit-insert-section--oldroot)
       (or (eq (magit-section-type section) 'unpushed)
           (equal (magit-section-value section) "@{upstream}..")
           (eq (magit-section-type section) 'stashes)
           (equal (magit-section-value section) "refs/stash"))
       'hide))

(defun magit-init-bare (directory)
  "Initialize a bare Git repository.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally."
  (interactive
   (let ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Create repository in: ")))))
     (-when-let (toplevel (magit-toplevel directory))
       (setq toplevel (expand-file-name toplevel))
       (unless (y-or-n-p (if (file-equal-p toplevel directory)
                             (format "Reinitialize existing repository %s? "
                                     directory)
                           (format "%s is a repository.  Create another in %s? "
                                   toplevel directory)))
         (user-error "Abort")))
     (list directory)))
  ;; `git init' does not understand the meaning of "~"!
  (magit-call-git "init" "--bare"
                  (magit-convert-filename-for-git
                   (expand-file-name directory))))

(defun wi-git-init+add-remote+push (source destination)
  "Initialize bare Git repository in DESTINATION directory,

Add local remote pointing to DESTINATION directory.

Push branch master to local/master."
  (interactive
   (list
    (read-directory-name "Source directory: ")
    (if wi-git wi-git
      (read-directory-name "Destination directory: "))))
  (let ((destination
         (concat (directory-file-name destination)
                 "/" (file-name-base
                      (directory-file-name (vc-git-root source))))))
    (magit-init-bare destination)
    (magit-remote-add "local" (concat "file://" destination))
    (magit-push "master" "local/master" nil)))

;; TODO: Another way will be in a new release,
;; see <https://emacs.stackexchange.com/a/38782/15092>.
;; (add-to-list 'magit-section-initial-visibility-alist '(stashes . hide))

(add-hook 'magit-section-set-visibility-hook
          'wi-local-magit-initially-hide-unmerged)

(add-hook 'git-commit-mode-hook 'auto-fill-mode)

;; Origin <https://github.com/alphapapa/unpackaged.el#improved-magit-status-command>.
(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (magit-status)
    (delete-other-windows)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error (cl-return (magit-status-goto-initial-section-1))))))))

;; Origin <https://github.com/alphapapa/unpackaged.el>
(defhydra unpackaged/smerge-hydra
  (:color pink :hint nil :post (smerge-auto-leave))
  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("ZZ" (lambda ()
          (interactive)
          (save-buffer)
          (bury-buffer))
   "Save and bury buffer" :color blue)
  ("q" nil "cancel" :color blue))

(add-hook 'magit-diff-visit-file-hook
          '(lambda ()
             (when smerge-mode
               (unpackaged/smerge-hydra/body))))


;;;
;;; ERC
;;;

;; Timestamps
(setq erc-insert-away-timestamp-function #'erc-insert-timestamp-left)
(setq erc-hide-timestamps nil)
(setq erc-timestamp-only-if-changed-flag nil)

(setq erc-accidental-paste-threshold-seconds 0.5)
(setq erc-autojoin-mode t)
(setq erc-autojoin-timing (quote ident))
(setq erc-email-userid user-mail-address)
(setq erc-flood-protect t)
(setq erc-join-buffer (quote bury))
(setq erc-query-display 'bury)
(setq erc-auto-query 'bury)
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
(setq erc-track-exclude-types
      '("NICK" "333" "353" "JOIN" "QUIT" "PART"))
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
  (add-to-list 'erc-networks-alist
               '(globalgamers "irc.globalgamers.net"))
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

(defvar wi-irc-gnome-channels
  '("#bugs" "#docs" "#gnome" "#gnome-hackers" "#gnome-shell"
    "#newcomers"))

(defun wi-erc-netlist (irc-networks irc-channels)
  (let (wi-erc-netlist)
    (dolist (irc-network irc-networks wi-erc-netlist)
      (if (equal wi-erc-netlist nil)
          (setq wi-erc-netlist
                (list (cons irc-network irc-channels)))
        (setq wi-erc-netlist
              (append wi-erc-netlist
                      (list (cons irc-network irc-channels))))))))

(defvar wi-erc-netlist-gnome (wi-erc-netlist wi-irc-gnome-servers
                                             wi-irc-gnome-channels))

(setq erc-autojoin-channels-alist
      (quote
       (("freenode.net" "#icecat" "#emacs" "#grub" "#conkeror" "#erc"
         ;; "#clojure" "##math"
         "##c" "#gdb" "#bash" "#SDL" "#chicken"
         ;; "#fedora" "#fedora-admin" "#fedora-devel"
         ;; "#fedora-noc" "#fedora-meeting" "#fedora-qa"
         "#gnu" "#fsf" "#gnus" "#guile" "#guix" "#stumpwm"
         "#bootstrappable"
         "#replicant"
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

(defcustom guix-irc-log-url "https://gnunet.org/bot/log/guix"
  "URL to IRC #guix channel log."
  :type 'string
  :group 'guix)

(defun guix-irc-open-log ()
  "Open IRC #guix channel log."
  (interactive)
  (browse-url guix-irc-log-url))


;;;
;;; Org
;;;

(setq org-descriptive-links nil)

;; (with-eval-after-load 'org
;;   (setq org-format-latex-options
;;         (plist-put org-format-latex-options :scale 1.5))
;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))

(with-eval-after-load 'org
  (require 'org-protocol) ; For `org-capture' from Xorg

  (add-to-list 'org-file-apps '("\\.png\\'" . system))

  (org-babel-do-load-languages
      'org-babel-load-languages
      '((R . t)
        (emacs-lisp . t)
        (scheme . t)
        (shell . t))))

(setq org-startup-folded 'showall) ; Show all in `org-mode' at startup

(setq org-email-link-description-format "Email %c: %s") ; More than 30 character

(setq org-capture-templates
      '(("c" "Note" item (file "~/.notes") "%?")

        ("f" "File email" entry (file+headline "inbox.org" "Email")
         "* %U %a by [[mailto:%:fromaddress][%:fromname]]"
         :immediate-finish nil
         :prepend nil)

        ;; Requires org-capture-extension
        ;; https://github.com/sprig/org-capture-extension
        ("l" "Protocol" item (file "web.org")
         "[[%:link][%:description]]\n%i"
         :immediate-finish t)
        ("L" "Protocol Link" item (file "web.org")
         "[[%:link][%:description]]"
         :immediate-finish t)

        ("X" "emacs-org-capture" item (file "web.org")
         "[[%:link]]"
         :immediate-finish t)

        ("r" "Respond ro email" entry (file+headline "inbox.org" "Email")
         "[[mailto:%:fromaddress][%:fromname]]"
         :immediate-finish t
         :prepend t)

        ("t" "Tasks" entry (file+headline ".notes" "Tasks")
         "* TODO %? \n%T" :prepend t)

        ("b" "buffer" entry (file "TODO.org") "* TODO [[%F]]%?")

        ("B" "blog" plain (file "blog.org") "%?")
        ("e" "emacs" plain (file "emacs.org") "%?")
        ("g" "guix" plain (file "guix.org") "%?")
        ("i" "Templates for TODO")
        ("ii" "TODO" entry (file "TODO.org") "* TODO %?")
        ("ir" "TODO region" entry (file "TODO.org") "* TODO %i"
         :immediate-finish t)
        ("m" "music" plain (file "music.org") "%?")
        ("n" "pdfview" item (file "pdf.org") "%a %?")
        ("o" "misc" plain (file "misc.org") "%?")
        ("p" "phrase" item (file "phrase.org") "%?")
        ("r" "read" plain (file "read.org") "%?")
        ("v" "video" plain (file "video.org") "%?")
        ("V" "watch" plain (file "watch.org") "%?")
        ("w" "work" plain (file "work.org") "%?")))

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/org/"
         :base-extension "org"
         :publishing-directory "~/public_html/org/"
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

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Origin <https://changelog.complete.org/archives/9865-emacs-2-introducing-org-mode>.
(setq org-ellipsis "…")


;;;
;;; ZNC
;;;

;; https://raw.githubusercontent.com/vincentbernat/dot.emacs/master/znc.conf.el

;;; Code:

(defun znc-setup (server port user networks)
  "Add a server to the list of ZNC servers.

We use SSL inconditionaly. Moreover, we don't store the password
but put nil instead. At least, we tweak the username to contain
the network name later, this will be separated again."
  (setq znc-servers
        (list (list server port
                    nil ;; SSL enabled
                    (mapcar (function (lambda (slug)
                                        (list slug
                                              (format "%s/%s" user slug)
                                              nil)))
                            networks)))))

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
(znc-setup "localhost" 8060 "natsu" '(freenode perl p2p twitch))


;;;
;;; EMMS
;;;

(with-eval-after-load 'emms-playlist-mode
  (require 'emms-setup)

  (with-eval-after-load 'emms-setup
    (emms-all)
    (emms-default-players)
    (setq emms-player-mpv-parameters
	  (append '("--no-terminal" "--force-window=no" "--audio-display=no"
                    "--no-resume-playback" "--keep-open=no"
                    "--audio-device=pulse/alsa_output.usb\
-Logitech_Logitech_USB_Headset-00.analog-stereo"
                    "--title=\"emacs-emms\"")
		  emms-player-mpv-parameters))

    (setq emms-volume-change-function #'emms-volume-pulse-change)
    (setq emms-player-next-function 'emms-next-noerror)
    (emms-mode-line -1)
    (setq emms-playing-time-display-p nil)
    (setq emms-playlist-mode-center-when-go t))

  (defmacro define-emms-advice-after (procedure after-procedure)
    `(progn
       (defun ,(intern (concat (symbol-name procedure)
                               "-"
                               (mapconcat 'identity
                                          (cdr (split-string (symbol-name after-procedure)
                                                             "-"))
                                          "-")))
           nil
         ,(format "Evaluate `%S' and call `%S'." procedure after-procedure)
         (interactive)
         (,procedure)
         (,after-procedure))))

  (define-emms-advice-after emms-next emms-playlist-mode-center-current)
  (define-emms-advice-after emms-previous emms-playlist-mode-center-current)
  (define-emms-advice-after emms-random emms-playlist-mode-center-current)
  (define-emms-advice-after emms-next emms-show)
  (define-emms-advice-after emms-previous emms-show)
  (define-emms-advice-after emms-random emms-show)

  (dolist (map (list emms-playlist-mode-map))
    (define-key map (kbd "n") 'emms-next-playlist-mode-center-current)
    (define-key map (kbd "p") 'emms-previous-playlist-mode-center-current)
    (define-key map (kbd "r") 'emms-random-playlist-mode-center-current)
    (define-key map (kbd "<backspace>") 'emms-player-simple-mpv-speed-normal)
    (define-key map (kbd "T") 'emms-player-simple-mpv-ontop)
    (define-key map (kbd "F") 'emms-player-simple-mpv-fullscreen)
    (define-key map (kbd "9") 'emms-volume-lower)
    (define-key map (kbd "0") 'emms-volume-raise))

  (with-eval-after-load 'emms-setup
    (setq emms-track-description-function (lambda (v) (assoc-default 'name v)))
    (setq emms-source-file-default-directory "/srv/music"))

  (with-eval-after-load 'helm-emms
    (require 'emms-setup)
    (setq helm-emms-use-track-description-function t)
    (add-to-list 'helm-emms-music-extensions "mkv")
    (add-to-list 'helm-emms-music-extensions "webm")))


;;;
;;; Stream
;;;

(defcustom wi-helm-wigust-stream-urls
  '("https://www.twitch.tv/nekrovim"
    "https://www.twitch.tv/entr_ru"
    "https://www.youtube.com/user/streamguild"
    "https://www.twitch.tv/artgameslp"
    "https://www.youtube.com/user/ArtGamesLP")
  "List of URLs passed to `wi-helm-wigust-stream'.")

(defun wi-helm-wigust-stream (func)
  "Open a streaming video URL in Chromium or Streamlink with Helm."
  (interactive (list
                (let ((engine (completing-read "Engine (chromium by default): "
                                               '(chromium streamlink))))
                  (if (string-empty-p engine)
                      "chromium"
                    engine))))
  (helm :sources (helm-build-sync-source "urls"
                   :action (lambda (candidate)
                             (funcall (cond ((string-equal func "chromium")
                                             'browse-url-chromium)
                                            ((string-equal func "streamlink")
                                             'browse-url-streamlink))
                                      candidate))
                   :candidates wi-helm-wigust-stream-urls
                   :fuzzy-match t)
        :buffer "*helm urls*"))

(defun erc-twitch ()
  "Open a current buffer with `browse-url-streamlink'"
  (interactive)
  (let ((url (concat "https://twitch.tv/"
                     (car (last (split-string (buffer-name (current-buffer))
                                              "#"))))))
    (message (concat "Open stream: " url))
    (browse-url-streamlink url)))


;;;
;;; RFC
;;;

(setq ffap-rfc-directories (list (expand-file-name "~/src/rfc")))

(setq irfc-directory (expand-file-name "~/src/rfc"))

(defcustom irfc-file-regexp
  (rx "rfc" (one-or-more numeric) ".txt")
  "Regexp matching RFC file."
  :group 'irfc
  :type 'regexp)

(add-to-list 'auto-mode-alist `(,irfc-file-regexp . irfc-mode))

(defvar irfc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") 'backward-char)
    (define-key map (kbd "l") 'forward-char)
    (define-key map (kbd "e") 'scroll-down)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "J") 'irfc-scroll-up-one-line)
    (define-key map (kbd "K") 'irfc-scroll-down-one-line)
    (define-key map (kbd ",") 'end-of-buffer)
    (define-key map (kbd ".") 'beginning-of-buffer)
    (define-key map (kbd "T") 'irfc-render-toggle)
    (define-key map (kbd "q") 'irfc-quit)
    (define-key map (kbd "o") 'irfc-follow)
    (define-key map (kbd "v") 'irfc-visit)
    (define-key map (kbd "r") 'irfc-reference-goto)
    (define-key map (kbd "g") 'irfc-head-goto)
    (define-key map (kbd "F") 'irfc-head-number-goto)
    (define-key map (kbd "G") 'irfc-page-goto)
    (define-key map (kbd "n") 'irfc-page-next)
    (define-key map (kbd "p") 'irfc-page-prev)
    (define-key map (kbd ">") 'irfc-page-last)
    (define-key map (kbd "<") 'irfc-page-first)
    (define-key map (kbd "b") 'irfc-page-table)
    (define-key map (kbd "N") 'irfc-head-next)
    (define-key map (kbd "P") 'irfc-head-prev)
    (define-key map (kbd "G") 'irfc-table-jump)
    (define-key map (kbd "<tab>") 'irfc-rfc-link-next)
    (define-key map (kbd "<backtab>") 'irfc-rfc-link-prev)
    map)
  "Keymap used by `irfc-mode'.")




;;;
;;; browse-url
;;;

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

(defcustom browse-url-mpv-program "mpv"
  "The name by which to invoke MPV."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-mpv-arguments '("--volume=50")
  "Arguments passed to mpv with `browse-url-mpv'."
  :type 'list
  :group 'browse-url)

(defcustom browse-url-mpv-headphones t
  "Non-nil if browse-url-mpv in headphones."
  :type 'boolean
  :group 'browse-url)

(defun toggle-browse-url-mpv-arguments ()
  "If browse-url-mpv-headphones non-nil set it to t and set
`browse-url-mpv-arguments' headphones."
  (interactive)
  (if browse-url-mpv-headphones
      (progn (setq browse-url-mpv-arguments '("--volume=50"))
             (setq browse-url-mpv-headphones nil))
    (setq browse-url-mpv-arguments
                (list "--volume=50" "--no-resume-playback"
                      "--keep-open=no"
                      (concat "--audio-device=" ‎wi-headphones)))
    (setq browse-url-mpv-headphones t))
  (message "MPV for headphones is %s"
	   (if browse-url-mpv-headphones "enabled" "disabled")))

(setq browse-url-mpv-remote-program "~/bin/mpv-remote")

(defun browse-url-mpv (url &optional new-window)
  "Ask the mpv video player to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-mpv-arguments' to mpv."
  (interactive (flet ((browse-url-url-at-point ; do not add `http://' prefix
                          () (or (thing-at-point 'url t)
                                 (let ((f (thing-at-point 'filename t)))
                                   f))))
                 (browse-url-interactive-arg "URL: ")))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "mpv " url) nil
           browse-url-mpv-program
           (append
            browse-url-mpv-arguments
            (list url)))))

(defun browse-url-chromium-no-toolbar (url &optional _new-window)
  "Ask the Chromium WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chromium-arguments' are also passed to
Chromium.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat "chromium " url) nil
	   browse-url-chromium-program
	   (append
	    browse-url-chromium-arguments
	    (list (concat "--app=" url))))))


;;;
;;; Outline
;;;

(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
(with-eval-after-load 'outshine
  (define-key outline-minor-mode-map (kbd "C-M-i") nil))

(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'scheme-mode-hook 'outline-minor-mode)


;;;
;;; Autotype
;;;

;; Inspired by https://github.com/suzp1984/donkey/blob/master/elisp/auto-insert/my-auto-insert.el

(defun yas-expand-current-buffer ()
  "Expand all yasnippet snippets in a current buffer."
  (interactive)
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(define-auto-insert
  (rx "package" (one-or-more (or alphanumeric "-")) ".scm" line-end)
  ["guix/gnu/packages/package.tmpl" yas-expand-current-buffer])

(define-auto-insert
  (rx "gnu/services/" (one-or-more (or alphanumeric "-")) ".scm" line-end)
  ["guix/gnu/services/service.tmpl" yas-expand-current-buffer])

(define-auto-insert
  (rx "vm" (one-or-more (or alphanumeric "-")) ".scm" line-end)
  ["guix/gnu/system/examples/vm-inherit-image.tmpl" yas-expand-current-buffer])

(define-auto-insert
  (rx "guile" (one-or-more (or alphanumeric "-")) line-end)
  ["guile/script.tmpl" scheme-mode yas-expand-current-buffer])

(defun wi-fullname-and-email ()
  (format "%s <%s>" user-full-name user-mail-address))

(define-skeleton copyright
  "Insert a copyright by $USER notice at cursor."
  "FULL_NAME <EMAIL>: "
  comment-start
  "; Copyright © " `(format-time-string "%Y") " "
  (or (wi-fullname-and-email) str)
  '(if (copyright-offset-too-large-p)
       (message "Copyright extends beyond `copyright-limit' and won't\
be updated automatically."))
  comment-end \n)

(setq copyright-names-regexp (wi-fullname-and-email))

(add-hook 'before-save-hook 'copyright-update)

(setq quickurl-format-function
      (lambda (url) (format "<%s>" (quickurl-url-url url))))


;;;
;;; Misc
;;;

(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

(autoload 'bash-completion-dynamic-complete
  "bash-completion" "BASH completion hook")

(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

(setq anywhere-kill-buffer nil)
(setq anywhere-major-mode 'text-mode)
(add-hooks '(((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . (lambda ()
                  (set (make-local-variable 'prettify-symbols-alist)
                       wi-scheme--prettify-symbols-alist)))
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . (lambda () (set-input-method "russian-computer")))
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . (lambda () (ispell-change-dictionary "ru")))
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . flyspell-mode)
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . abbrev-mode)
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . yas-minor-mode)
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . (lambda ()
                  (setq-local company-idle-delay 0.1)
                  (setq-local company-minimum-prefix-length 2)))
             ((anywhere-mode-hook atomic-chrome-edit-mode-hook)
              . visual-line-mode)))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-abbrev))

(with-eval-after-load 'anywhere-mode
  (let ((map anywhere-mode-map))
    (define-key map (kbd "C-c '") 'anywhere-exit)
    (define-key map (kbd "C-c i") 'ispell-buffer)
    (define-key map (kbd "C-c v") 'ivy-yasnippet)))

(with-eval-after-load 'atomic-chrome
  (let ((map atomic-chrome-edit-mode-map))
    (define-key map (kbd "C-c '") 'anywhere-exit)
    (define-key map (kbd "C-c i") 'ispell-buffer)
    (define-key map (kbd "C-c v") 'ivy-yasnippet)))

(add-hook 'find-file-hook 'auto-insert)

(setq redshift-temp-increment 100)

(setq youtube-stream-open-chat-function
      #'browse-url-chromium-no-toolbar)

(setq youtube-stream-open-video-function #'browse-url-streamlink)

;; Origin <https://github.com/Wilfred/.emacs.d/blob/gh-pages/init.org>.
(setq enable-recursive-minibuffers t) ; Enable recursive minibuffer.
(minibuffer-depth-indicate-mode)      ; Show recursion depth.

;; Origin <https://github.com/Wilfred/.emacs.d/blob/gh-pages/init.org>.
(setq scroll-preserve-screen-position 'always) ; Preserve scroll pos.

(defun wi-shell-cd-current-dir ()
  "Invoke shell and cd to `default-directory'."
  (interactive)
  (let ((dir default-directory))
    (shell)
    (insert "cd " dir)
    (comint-send-input)))

(dolist (elt '((compile . compilation-mode-map)
               (grep . grep-mode-map)
               (ivy . ivy-occur-grep-mode-map)
               (ag . ag-mode-map)
               (ack . ack-mode-map)))
  (eval-after-load (car elt)
    `(progn
       (define-key ,(cdr elt) (kbd "+")
         #'grep-context-more-around-point)
       (define-key ,(cdr elt) (kbd "-")
         #'grep-context-less-around-point))))

(setq pulseaudio-control-pactl-path
      "/run/current-system/profile/bin/pactl")

(defun wi-copy-buffer (buffer)
  "Copy BUFFER to kill ring and save in the GUI’s clipboard."
  (with-current-buffer (get-buffer buffer)
    (save-excursion
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun wi-copy-current-buffer ()
  "Copy current buffer to kill ring and save in the GUI’s clipboard."
  (interactive)
  (wi-copy-buffer (current-buffer)))

(setq dumb-jump-max-find-time 4)

(defun wi-replace-with-brackets-ellipsis ()
  "Replace region with \"[…]\"."
  (interactive)
  (kill-region (region-beginning) (region-end))
  (insert "[…]")
  (newline 2))

(defun wi-ttn-hs-hide-level-1 ()
  (hs-hide-level 1)
  (forward-sexp 1))

(setq hs-hide-all-non-comment-function 'wi-ttn-hs-hide-level-1)

(defcustom wi-scheme-mode-toggle-hs-minor-mode nil
  "If non-nil enable `hs-minor-mode' in `scheme-mode'."
  :type 'boolean)

(defun wi-scheme-mode-toggle-hs-minor-mode ()
  (interactive)
  (if wi-scheme-mode-toggle-hs-minor-mode
      (progn (remove-hook 'scheme-mode-hook 'hs-minor-mode)
             (setq wi-scheme-mode-toggle-hs-minor-mode nil))
    (progn (add-hook 'scheme-mode-hook 'hs-minor-mode)
           (add-hook 'scheme-mode-hook #'hs-hide-all)
           (setq wi-scheme-mode-toggle-hs-minor-mode t))))

;; Origin <https://www.emacswiki.org/emacs/RecentFiles>.

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(with-eval-after-load 'tramp
  (recentf-mode 1))
(setq recentf-max-menu-items 500)
(setq recentf-max-saved-items nil)
(setq recentf-auto-cleanup 'never)

(setq hl-sexp-background-color "darkseagreen2")

(when (and (require 'edit-server nil t) (daemonp))
  (edit-server-start))

(setq terminal-here-scrollbar nil)
(setq terminal-here-terminal-emulators (list "xterm"))
(setq-default terminal-here-project-root-function #'projectile-project-root)

;; See <https://www.emacswiki.org/emacs/DoWhatIMean>
(setq dired-dwim-target t)

(blink-cursor-mode)


;;;
;;; Theme
;;;

(defun wi-manoj-dark ()
  (interactive)
  (load-theme 'manoj-dark)
  (custom-theme-set-faces
   'manoj-dark
   '(font-lock-function-name-face ((t (:foreground "mediumspringgreen" :weight bold :height 1.0))))
   '(diff-refine-added ((t (:inherit diff-refine-change :background "#22aa22" :foreground "aquamarine1"))))
   '(diff-refine-removed ((t (:inherit diff-refine-change :background "#aa2222" :foreground "plum1"))))
   '(which-key-command-description-face ((t (:inherit font-lock-function-name-face :height 1.0))))
   '(fringe ((t (:background "black" :foreground "Wheat"))))
   '(header-line
     ((t (:background "black" :foreground "grey90" :height 0.9))))
   ;; '(scroll-bar ((t (:background "black" :foreground "WhiteSmoke"))))
   ;; '(mode-line ((t (:background "WhiteSmoke" :foreground "black"))))
   ;; '(mode-line-inactive ((t (:background "black" :box nil))))
   ;; '(mode-line-buffer-id ((t (:background "grey15" :foreground "red"))))
   '(mode-line ((t (:background "gray18" :foreground "white" :inverse-video nil :box (:line-width 1 :color "black") :height 1.0))))
   '(mode-line-inactive ((t (:background "black" :foreground "white" :inverse-video nil :box (:line-width 1 :color "black") :weight light :height 1.0))))
   '(mode-line-buffer-id ((t (:background "black" :foreground "red"))))
   '(elfeed-search-title-face ((t (:foreground "dim gray"))))
   '(elfeed-search-unread-title-face ((t (:foreground "white"))))
   '(completions-common-part ((t (:width normal :weight normal
                                :slant normal :foreground "gold1"
				:background "black"))))
   '(highlight-stages-level-1-face ((t (:background "dark slate gray"))))
   '(highlight-stages-level-2-face ((t (:background "dark olive green"))))
   '(highlight-stages-level-3-face ((t (:background "sea green"))))
   '(highlight-stages-negative-level-face ((t (:background "dark cyan"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   '(next-error ((t (:background "steel blue"))))
   '(region ((t (:background "steel blue")))))
  (setq sml/theme 'dark)
  (sml/setup))

(defvar wi-theme t
  "If non-nil use dark theme.

If nil use light theme.")

(defun wi-toggle-theme ()
  "Toggle between dark and light themes."
  (interactive)
  (if wi-theme
      (progn (mapc (lambda (theme)
                     (disable-theme theme))
                   '(manoj-dark smart-mode-line-dark))
             (enable-theme 'smart-mode-line-light)
             (setq wi-theme nil))
    (progn (disable-theme 'smart-mode-line-light)
           (wi-manoj-dark)
           (setq wi-theme t))))


;;;
;;; smart-mode-line
;;;

(setq sml/no-confirm-load-theme t)
(when (daemonp) (setq sml/theme 'light))
(sml/setup)

(default-text-scale-mode)
(global-git-gutter-mode)

(defun wi-copy-file-name ()
  "Return current buffer file name."
  (interactive)
  (kill-new (buffer-file-name)))

(defun wi-copy-project-file-name ()
  "Return current buffer file name in current project."
  (interactive)
  (kill-new (file-relative-name (buffer-file-name)
                                (funcall (cl-find-if 'fboundp
                                                     '(projectile-project-root
                                                       vc-root-dir))))))

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
     shell-mode-hook ibuffer-mode-hook guix-env-var-mode-hook)
    . guix-prettify-mode)
   ((c-mode-hook) . ggtags-mode)
   ((dired-mode-hook) . toggle-truncate-lines)
   ((scheme-mode-hook) . guix-devel-mode)
   ((prog-mode-hook geiser-repl-mode-hook)
    . rainbow-delimiters-mode)
   ((geiser-repl-mode-hook shell-mode-hook)
    . (lambda ()
        (setq bidi-display-reordering nil)))
   ((geiser-repl-mode-hook shell-mode-hook)
    . (lambda ()
        (font-lock-mode -1)))
   ((prog-mode-hook
     minibuffer-inactive-mode-hook
     geiser-repl-mode-hook
     git-commit-mode-hook
     org-mode-hook)
    . smartparens-strict-mode)
   ((prog-mode-hook org-mode-hook text-mode-hook) . yas-minor-mode)
   ((prog-mode-hook) . hs-minor-mode)
   ((erc-mode-hook) . (lambda () (setq truncate-lines t)))))

(defun wi-find-stumpwm-init-file ()
  "Edit the `stumpwm-init-file', in another window."
  (interactive)
  (find-file-other-window
   (expand-file-name "~/.stumpwm.d/init.lisp")))

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
    (cond
     (god-local-mode
      (progn
        (set-face-background
         'mode-line
         (if limited-colors-p "white" "#e9e2cb"))
        (set-face-background
         'mode-line-inactive
         (if limited-colors-p "white" "#e9e2cb"))))
     (t (progn
          (set-face-background
           'mode-line
           (if limited-colors-p "black" "grey75"))
          (set-face-background
           'mode-line-inactive
           (if limited-colors-p "grey20" "grey90")))))))

(add-hook 'god-mode-enabled-hook 'wi-god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'wi-god-mode-update-cursor)

(setq mml-secure-insert-signature 'always)

(add-hook 'prog-mode-hook 'hl-todo-mode)

(add-hook 'shell-mode-hook
          (lambda ()
            (progn (setq paragraph-separate "[ 	]*$")
                   (setq paragraph-start "\\|[ 	]*$"))))

(load (expand-file-name "~/.emacs.d/erza/elfeed.el"))

;; `w3m' fonts
(setq w3m-fill-column 80)

;; TODO: debpaste API broken
;; (setq debpaste-user-name "wigust")

;; `eww' fonts
(setq shr-width 80)
(with-eval-after-load 'elfeed
  (defun wi-elfeed-search-show-entry ()
    "Call `elfeed-search-show-entry' with `shr-width' setted to NIL."
    (interactive)
    (let ((shr-width nil))
      (call-interactively 'elfeed-search-show-entry)))
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "RET") 'wi-elfeed-search-show-entry)))

(setq-default shr-use-fonts nil)
(setq shr-external-browser 'browse-url-conkeror)

;; Not white background in dark themes.
;; Origin <https://emacs.stackexchange.com/a/3523>
(setq shr-color-visible-luminance-min 100)
(advice-add #'shr-colorize-region
            :around (defun shr-no-colourise-region (&rest ignore)))

;; Toggle show-paren-mode on
(show-paren-mode)

;; Don't use ido
(setq projectile-completion-system 'default)

(setq helm-locate-project-list (f-directories wi-src))

;; Google translate with translate-shell program
(require 'google-translate-mode nil t)
(with-eval-after-load 'google-translate-mode
  (setq trans-target "ru"))

;; Interested in those timezones
(with-eval-after-load 'time
  (setq display-time-world-time-format "%Z\t%a\t%d %B %H:%M")
  (setq display-time-world-list
        '(("Europe/Moscow" "Europe/Moscow")
          ("Europe/Berlin" "Europe/Berlin")
          ("Europe/London" "Europe/London")
          ("Europe/Istanbul" "Europe/Istanbul")
          ("America/Winnipeg" "America/Winnipeg")
          ("America/New_York" "America/New_York")
          ("Asia/Tokyo" "Asia/Tokyo")
          ("Asia/Bangkok" "Thailand"))))

;; List of Email addresses to send patches for `gitpatch-mail' command
(setq gitpatch-mail-database (list "guix-patches@gnu.org"))

(save-place-mode)            ; Remember position in files
(setq mouse-yank-at-point t) ; Ignore mouse position on paste
(setq vc-follow-symlinks t)  ; Do not ask about following link in Git projects
(setq dired-listing-switches (purecopy "-alh")) ; Prettify dired
(setq dired-hide-details-hide-symlink-targets nil)

;; Toggle prettify symbols mode on
(global-prettify-symbols-mode)

 ; Unprettify symbol after the cursor
(setq prettify-symbols-unprettify-at-point 'right-edge)

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
  (require 'smartparens-config nil t)
  (setq sp-highlight-pair-overlay nil)
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

(cl-defun wi-debbugs-gnu-list (&optional (mail-address user-mail-address)
                                         (not-suppress nil))
  "List bugs on debbugs.gnu.org from USER-MAIL-ADDRESS.

With NOT-SUPPRESS non-nil argument include archived bugs."
  (interactive)
  (let ((debbugs-gnu-current-query `((submitter . ,mail-address))))
    (if (or current-prefix-arg not-suppress)
        (debbugs-gnu nil nil nil nil)
        (debbugs-gnu nil nil nil t))))

(setq ewmctrl-wmctrl-path "/run/current-system/profile/bin/wmctrl")

(defun wi-calendar-current-date-time ()
  "Return the formated string of current year month day hour minute."
  (interactive)
  (let* ((now (decode-time))
         (year (nth 5 now))
         (month (nth 4 now))
         (day (nth 3 now))
         (hour (nth 2 now))
         (minute (nth 1 now)))
    (kill-new (format "%d-%d-%d %d:%d" year month day hour minute))))

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
      (insert (shell-command-to-string
               (mapconcat 'identity (list "wget" "-q" "-O-" url)
                          " ")))
      (special-mode))
    (switch-to-buffer buffer)))

(defun wi-debbugs-get-url (bug-number)
  "Get a debbugs url according to `BUG-NUMBER'"
  (interactive "sBug number: ")
  (kill-new (concat "https://debbugs.gnu.org/cgi/bugreport.cgi?bug="
                    bug-number)))

(defun wi-copy-cgit-guix-path (path)
  "Copy cgit guix path to kill ring"
  (interactive "sPath: ")
  (kill-new (concat "https://git.savannah.gnu.org/cgit/guix.git/tree/"
                    path)))

(defun wi-info-remote-copy-current-node ()
  "Copy URL to current Info node."
  (interactive)
  (kill-new
   (concat "https://www.gnu.org/software/"
           (file-name-sans-extension
            (file-name-nondirectory Info-current-file))
           "/manual/html_node/"
           (let ((split-str (split-string Info-current-node " ")))
             (if (> (length split-str) 1)
                 (mapconcat 'identity split-str "-")
               Info-current-node))
           ".html")))

(defvar wi-guix-git-directory (expand-file-name "~/src/guix"))
(defun wi-magit-show-commit-guix (commit)
  "Show a Git `commit' from the Guix checkout.

If no commit hash provides, show a commit from hash at current point."
  (interactive (list (read-string "Commit: " nil nil (word-at-point))))
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

(defvar wi-github-url-regexp
  (rx "http" (zero-or-one "s") "://github.com"))

(defvar wi-github-user-url-regexp
  (concat wi-github-url-regexp
          (rx "/" letter (one-or-more alphanumeric))))

(defvar wi-github-user-repo-url-regexp
  (concat wi-github-user-url-regexp
          (rx "/" (one-or-more (or alphanumeric "-" ".")))))

(defvar wi-github-user-repo-commit-url-regexp
  (concat wi-github-user-repo-url-regexp
          (rx "/commit" "/" (one-or-more alphanumeric))))

(defun wi-clipboard-github-url-to-commit (url)
  "Return in kill ring a commit hash from GitHub user's repository
commit URL.

https://github.com/USER/REPO/commit/SHA1-HASH => SHA1-HASH"
  (interactive
   (let ((clipboard (x-get-clipboard)))
     (list
      (if (string-match-p wi-github-user-repo-commit-url-regexp
                          clipboard)
          clipboard
        (read-string "Github user's repository commit URL: ")))))
  (kill-new (car (last (split-string url "/")))))

(add-to-list 'projectile-project-root-files "environment-variables")

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

(defun wi-find-docker-gitlab-file-name ()
  (interactive)
  (let ((file-name (split-string (buffer-file-name) "/")))
    (find-file
     (mapconcat 'identity
                (if (string= (nth 1 file-name) "docker:gitlab:")
                    `("/srv/src/gitlab-ce" ,@(nthcdr 7 file-name))
                    `("/docker:gitlab:/opt/gitlab/embedded/service/gitlab-rails"
                      ,@(nthcdr 4 file-name)))
                "/"))))

(defun wi-compile-guix (directory)
  ""
  (interactive "DDirectory: ")
  (require 'compile)
  (mapc 'kill-process compilation-in-progress)
  (compile
   (format
    "cd %s; guix environment --pure guix --ad-hoc help2man guile-sqlite3 guile-gcrypt -- make -j4 -k"
    directory)))

(defun wi-debbugs-gnu-apply-patch (&optional branch)
  "Apply the patch from the current message.
If given a prefix, patch in the branch directory instead."
  (interactive "P")
  (add-hook 'emacs-lisp-mode-hook 'debbugs-gnu-lisp-mode)
  (add-hook 'diff-mode-hook 'debbugs-gnu-diff-mode)
  (add-hook 'change-log-mode-hook 'debbugs-gnu-change-mode)
  (debbugs-gnu-init-current-directory branch)
  (let ((rej (expand-file-name "debbugs-gnu.rej" temporary-file-directory))
	(output-buffer (get-buffer-create "*debbugs patch*"))
	(patch-buffers nil))
    (when (file-exists-p rej)
      (delete-file rej))
    (with-current-buffer output-buffer
      (erase-buffer))
    (gnus-summary-select-article nil t)
    ;; The patches are either in MIME attachements or the main article
    ;; buffer.  Determine which.
    (with-current-buffer gnus-article-buffer
      (dolist (handle (mapcar 'cdr (gnus-article-mime-handles)))
	(when
	    (string-match "diff\\|patch\\|plain" (mm-handle-media-type handle))
	  (push (cons (mm-handle-encoding handle)
		      (mm-handle-buffer handle))
		patch-buffers))))
    (unless patch-buffers
      (gnus-summary-show-article 'raw)
      (article-decode-charset)
      (push (cons nil gnus-article-buffer) patch-buffers))
    (dolist (elem patch-buffers)
      (with-current-buffer (generate-new-buffer "*debbugs input patch*")
	(insert-buffer-substring (cdr elem))
	(cond ((eq (car elem) 'base64)
	       (base64-decode-region (point-min) (point-max)))
	      ((eq (car elem) 'quoted-printable)
	       (quoted-printable-decode-region (point-min) (point-max))))
	(debbugs-gnu-fix-patch debbugs-gnu-current-directory)
	(call-process-region (point-min) (point-max)
			     "patch" nil output-buffer nil
			     "-r" rej "--no-backup-if-mismatch"
			     "-l" "-f"
			     "-d" (expand-file-name
				   debbugs-gnu-current-directory)
			     "-p1")))
    (set-buffer output-buffer)
    (when (file-exists-p rej)
      (goto-char (point-max))
      (insert-file-contents-literally rej))
    (goto-char (point-max))
    (save-some-buffers t)
    (wi-compile-guix (expand-file-name debbugs-gnu-current-directory))
    (vc-dir debbugs-gnu-current-directory)
    (vc-dir-hide-up-to-date)
    (goto-char (point-min))
    (sit-for 1)
    (vc-diff)
    ;; All these commands are asynchronous, so just wait a bit.  This
    ;; should be done properly a different way.
    (sit-for 2)
    ;; We've now done everything, so arrange the windows we need to see.
    (delete-other-windows)
    (switch-to-buffer output-buffer)
    (split-window)
    (split-window)
    (other-window 1)
    (switch-to-buffer "*compilation*")
    (goto-char (point-max))
    (other-window 1)
    (switch-to-buffer "*vc-diff*")
    (goto-char (point-min))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(build-farm-build-arguments '("system=x86_64-linux"))
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(debug-on-error nil)
 '(indent-tabs-mode nil)
 '(safe-local-variable-values
   '((git-auto-commit-mode . t)
     (eval progn
           (put 'with-directory 'scheme-indent-function 1)
           (put 'with-repository 'scheme-indent-function 2))
     (TeX-master . "guile.texi")
     (eval c-set-offset 'access-label '-)
     (eval c-set-offset 'substatement-open 0)
     (eval c-set-offset 'arglist-cont-nonempty '+)
     (eval c-set-offset 'arglist-cont 0)
     (eval c-set-offset 'arglist-intro '+)
     (eval c-set-offset 'inline-open 0)
     (eval c-set-offset 'defun-open 0)
     (eval c-set-offset 'innamespace 0)
     (indicate-empty-lines . t)
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (bug-reference-bug-regexp . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-even-diff-B ((t (:background "gray35"))))
 '(ediff-odd-diff-A ((t (:background "gray35"))))
 '(ediff-odd-diff-B ((t (:background "gray35"))))
 '(fringe ((t nil)))
 '(irfc-head-name-face ((t (:inherit font-lock-function-name-face))))
 '(irfc-head-number-face ((t (:inherit font-lock-function-name-face))))
 '(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(irfc-rfc-link-face ((t (:inherit link))))
 '(magit-diff-added ((t (:foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:foreground "#22aa22"))))
 '(magit-diff-context-highlight ((t (:foreground "grey50"))))
 '(magit-diff-removed ((t (:foreground "#aa2222"))))
 '(magit-diff-removed-highlight ((t (:foreground "#aa2222"))))
 '(markdown-code-face ((t (:inherit fixed-pitch))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "goldenrod"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "lime green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "goldenrod"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
 '(sh-heredoc ((t (:foreground "tan3")))))
