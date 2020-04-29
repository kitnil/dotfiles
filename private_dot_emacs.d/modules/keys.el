(global-set-key (kbd "<f9>") 'vterm-toggle-cd)

;;; See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html>.
;;; Watch about hydra <https://www.youtube.com/watch?v=_qZliI1BKzI>.
(defmacro wi-define-keys (prefix prefix-map &rest args)
  "Define keys.
PREFIX - prefix key for these bindings.
PREFIX-MAP - prefix key for these bindings.

Sets the following basend on PREFIX-MAP:
- which - description of these bindings.
- hydra - hydra function name.
- hydra-comment - hydra description of these bindings.

ARGS will be passed to hydra."
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

(wi-define-keys "C-c g v t" timemachine
                ("t" git-timemachine "timemachine"))

(wi-define-keys "C-c g v m" magit
                ("c" magit-commit "commit")
                ("f" magit-file-dispatch "dispatch")
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
                ("d" dumb-jump-go "go")
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
                ("r" helm-recentf "recent" :color blue)
                ("m" helm-make "make" :color blue)
                ("c" helm-resume "resume" :color blue)
                ("s" helm-pass "pass" :color blue)
                ("t" helm-top "top" :color blue)
                ("v" wi-helm-wigust-stream "stream" :color blue)
                ("w" helm-stumpwm-commands "stumpwm" :color blue)
                ("x" helm-M-x "M-x" :color blue)
                ("y" helm-show-kill-ring "kill ring" :color blue)
                ("8" helm-ucs "character" :color blue))

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
                ("x" counsel-M-x "M-x" :color blue)
                ("w" plain-org-wiki "org-wiki" :color blue))

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

;; https://endlessparentheses.com/keep-your-slack-distractions-under-control-with-emacs.html
(wi-define-keys "C-c g m s" slack
                ("s" slack-select-rooms "select-room"))

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
                ("r" counsel-tramp :color blue)
                ("e" eshell "eshell" :color blue)
                ("m" terminal-here-project-launch-multiplexer "xterm" :color blue)
                ("t" term "ansi" :color blue)
                ("x" terminal-here-project-launch "xterm" :color blue))

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

;; Encrypt Email message with Gnupg
(with-eval-after-load 'message
  (require 'jl-encrypt)
  (add-hook 'message-setup-hook 'mml-secure-encrypt-if-possible)
  (let ((map message-mode-map))
    (define-key map (kbd "C-c s") 'ispell-region)
    (define-key map (kbd "C-c e") 'wi-replace-with-brackets-ellipsis)))

(with-eval-after-load 'elisp-mode
  (let ((map emacs-lisp-mode-map))
    (define-key map (kbd "C-c C-z") 'wi-switch-to-scratch-elisp)))

(with-eval-after-load 'slime
  (let ((map slime-mode-map))
    (define-key map (kbd "C-c C-z") 'scratch)))

(with-eval-after-load 'view
  (let ((map view-mode-map))
    (define-key map (kbd "<f8>") 'delete-current-buffer-file)))

(let ((map text-mode-map))
  (define-key map (kbd "<f7>") 'aya-create)
  (define-key map (kbd "<f8>") 'aya-expand))

(with-eval-after-load 'prog-mode
  (let ((map prog-mode-map))
    (define-key map (kbd "<f5>") 'recompile)
    (define-key map (kbd "<f6>") 'rg)
    (define-key map (kbd "<f7>") 'highlight-symbol-prev)
    (define-key map (kbd "<f8>") 'highlight-symbol-next)))

(with-eval-after-load 'perl-mode
  (let ((map perl-mode-map))
    (define-key map (kbd "M-.") 'dumb-jump-go)))

(with-eval-after-load 'nix-mode
  (let ((map nix-mode-map))
    (define-key map (kbd "M-.") 'dumb-jump-go)
    (define-key map (kbd "C-c C-z") 'nix-repl)))

;; (with-eval-after-load 'c-mode
;;   (let ((map c-mode-map))
;;     (define-key map (kbd "M-.") 'dumb-jump-go)))

;; (with-eval-after-load 'java-mode
;;   (let ((map java-mode-map))
;;     (define-key map (kbd "M-.") 'dumb-jump-go)))

;; (with-eval-after-load 'groovy-mode
;;   (let ((map groovy-mode-map))
;;     (define-key map (kbd "M-.") 'dumb-jump-go)))

;; (with-eval-after-load 'js-mode
;;   (let ((map js-mode-map))
;;     (define-key map (kbd "M-.") 'dumb-jump-go)))

;; (with-eval-after-load 'php-mode
;;   (let ((map php-mode-map))
;;     (define-key map (kbd "M-.") 'dumb-jump-go)))

(with-eval-after-load 'gnus-art
  (let ((map gnus-article-mode-map))
    (define-key map (kbd "C-c c") 'wi-magit-show-commit-guix)
    (define-key map (kbd "C-c d") 'wi-gnus-browse-debbugs)
    (define-key map (kbd "C-c b") 'wi-gnus-browse-guix-issues)))

(with-eval-after-load 'grep
  (let ((map grep-mode-map))
    (define-key map (kbd "C-j") 'compile-goto-error)))

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

