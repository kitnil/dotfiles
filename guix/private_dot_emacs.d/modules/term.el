;; https://github.com/syl20bnr/spacemacs/issues/6181
;; Avoid 2 second delay in emacsclient startup with Emacs 24.3.50+ and GNU screen 
;;
;; (eval-after-load "xterm" ;; term/xterm.el does not provide 'xterm
;;   '(defadvice xterm--query (around tweak-for-gnu-screen (query handlers) activate)
;;      ;; GNU screen does not support this sequence
;;      (unless (string= query "\e]11;?\e\\")
;;        ad-do-it)))
;;
(setq-default xterm-query-timeout nil)

(setq terminal-here-scrollbar nil)
(setq terminal-here-terminal-emulators (list "alacritty" "xterm"))
(setq-default terminal-here-project-root-function #'projectile-project-root)
(setq terminal-here-multiplexers '("tmux" "screen"))

(defun wi-shell-current-dir ()
  "Open shell in current buffer directory."
  (interactive)
  (shell (concat "*shell " default-directory "*")))

(defun wi-shell-cd-current-dir ()
  "Invoke shell and cd to `default-directory'."
  (interactive)
  (let ((dir default-directory))
    (shell)
    (insert "cd " dir)
    (comint-send-input)))

(add-hook 'shell-mode-hook
          (lambda ()
            (progn (setq paragraph-separate "[ 	]*$")
                   (setq paragraph-start "\\|[ 	]*$"))))

(add-hook 'shell-mode-hook
          (lambda ()
            (set-process-query-on-exit-flag (get-process "shell") nil)))

(add-hook 'eshell-mode-hook 'eshell-bookmark-setup)

;; (add-hook 'comint-output-filter-functions
;;           'comint-truncate-buffer)

(with-eval-after-load 'vterm
  (let ((map vterm-mode-map))
    (define-key map (kbd "<S-prior>") 'scroll-down-command)
    (define-key map (kbd "<S-next>") 'scroll-up-command)
    (define-key map (kbd "C-u") 'vterm-send-C-u)
    (define-key map (kbd "C-y") 'vterm-send-C-y)
    (define-key map (kbd "<C-backspace>") 'vterm-send-backspace)
    (define-key map (kbd "<menu>") 'vterm-copy-mode)
    (define-key map (kbd "M-/") 'vterm-dabbrev-expand)
    (define-key map (kbd "S-<right>") 'multi-vterm-next)
    (define-key map (kbd "S-<left>") 'multi-vterm-prev))
  (let ((map vterm-copy-mode-map))
    (define-key map (kbd "<menu>") 'vterm-copy-mode))
  (push (list "find-file-below"
              (lambda (path)
                (if-let* ((buf (find-file-noselect path))
                          (window (display-buffer-below-selected buf nil)))
                    (select-window window)
                  (message "Failed to open file: %s" path))))
        vterm-eval-cmds))

(defun wi-vterm-prettify-symbols ()
  (when (boundp 'mjru-prettify-hosts)
    (set (make-local-variable 'prettify-symbols-alist)
         (append `(("&&" . ?∧)
                   ("||" . ?∨)
                   ("++" . ?⧺)
                   ("<=" . ?≤)
                   (">=" . ?≥)
                   ("->" . ?→))
                 mjru-prettify-hosts))))

(add-hook 'vterm-mode-hook 'wi-vterm-prettify-symbols)

(add-hook 'vterm-mode-hook 'goto-address-mode)
(add-hook 'vterm-mode-hook 'guix-prettify-mode)
(when (boundp #'emojify-mode)
  (add-hook 'vterm-mode-hook 'emojify-mode))

(setq vterm-max-scrollback 100000)
(setq vterm-min-window-width 110)

(defun vterm-dabbrev-expand-wrapper ()
  (save-current-buffer
    (setq-local buffer-read-only nil)
    (call-interactively #'dabbrev-expand)
    (thing-at-point 'symbol)))

(defun vterm-dabbrev-expand ()
  (interactive)
  (if (thing-at-point 'symbol)
      (progn (vterm-send-C-w)
             (vterm-send-string (vterm-dabbrev-expand-wrapper) t))
    (vterm-send-string (vterm-dabbrev-expand-wrapper) t)))


;;;
;;; shell
;;;

(setq shell-kill-buffer-on-exit t)


;;;
;;; eat
;;;

;; https://codeberg.org/akib/emacs-eat/issues/145
;; Workaround for eat scrolling issues; disable recentering.
(add-hook 'eat-mode-hook (lambda () (setq-local scroll-conservatively 101)))

;;; esc always quits
(with-eval-after-load 'eat
  (mapcar (lambda (mode-map)
            (define-key mode-map [escape] 'eat-self-input))
          (list eat-char-mode-map
                eat-eshell-char-mode-map
                eat-eshell-emacs-mode-map
                eat-eshell-semi-char-mode-map
                eat-line-mode-map
                eat-mode-map
                eat--mouse-click-mode-map
                eat--mouse-modifier-click-mode-map
                eat--mouse-movement-mode-map
                eat-semi-char-mode-map
                eat-trace-replay-mode-map)))

;; Don't whine if there is a terminal open.
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(add-hook 'eat-exec-hook (lambda (&rest_) (set-no-process-query-on-exit)))

(add-hook 'eat-exec-hook (lambda (&rest_) (eat-char-mode)))

(defun kill-buffer-on-process-exit-status-0 (process)
  (when (= (process-exit-status process) 0)
    (kill-buffer-and-frame)))

(add-hook 'eat-exit-hook #'kill-buffer-on-process-exit-status-0)

(defun wi-eat-prettify-symbols ()
  (set (make-local-variable 'prettify-symbols-alist)
       `(("&&" . ?∧)
         ("||" . ?∨)
         ("++" . ?⧺)
         ("<=" . ?≤)
         (">=" . ?≥)
         ("->" . ?→))))

(add-hook 'eat-mode-hook 'wi-eat-prettify-symbols)
