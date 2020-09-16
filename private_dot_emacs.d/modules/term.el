(setq terminal-here-scrollbar t)
(setq terminal-here-terminal-emulators (list "xterm"))
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

(autoload 'bash-completion-dynamic-complete
  "bash-completion" "BASH completion hook")

(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

(with-eval-after-load 'vterm
  (let ((map vterm-mode-map))
    (define-key map (kbd "<S-prior>") 'scroll-down-command)
    (define-key map (kbd "<S-next>") 'scroll-up-command)
    (define-key map (kbd "C-u") 'vterm-send-C-u)
    (define-key map (kbd "C-y") 'vterm-send-C-y)
    (define-key map (kbd "<C-backspace>") 'vterm-send-backspace)
    (define-key map (kbd "<menu>") 'vterm-copy-mode)
    (define-key map (kbd "M-/") 'vterm-dabbrev-expand))
  (let ((map vterm-copy-mode-map))
    (define-key map (kbd "<menu>") 'vterm-copy-mode))
  (push (list "find-file-below"
              (lambda (path)
                (if-let* ((buf (find-file-noselect path))
                          (window (display-buffer-below-selected buf nil)))
                    (select-window window)
                  (message "Failed to open file: %s" path))))
        vterm-eval-cmds))

(defvar wi-vterm--prettify-symbols-alist
  '(("&&" . ?∧)
    ("||" . ?∨)
    ("++" . ?⧺)
    ("<=" . ?≤)
    (">=" . ?≥)))

(add-hook 'vterm-mode-hook
          (lambda ()
            (set (make-local-variable 'prettify-symbols-alist)
                 wi-vterm--prettify-symbols-alist)))

(setq vterm-max-scrollback 100000)
(setq vterm-min-window-width 160)

(defun vterm-dabbrev-expand-wrapper ()
  (save-current-buffer
    (setq-local buffer-read-only nil)
    (call-interactively #'dabbrev-expand)
    (thing-at-point 'symbol)))

(defun vterm-dabbrev-expand ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol
        (progn (dotimes (i (length symbol))
                 (vterm-send-backspace))
               (vterm-send-string (vterm-dabbrev-expand-wrapper) t))
      (vterm-send-string (vterm-dabbrev-expand-wrapper) t))))
