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

(add-hook 'eshell-mode-hook 'eshell-bookmark-setup)

;; (add-hook 'comint-output-filter-functions
;;           'comint-truncate-buffer)

(autoload 'bash-completion-dynamic-complete
  "bash-completion" "BASH completion hook")

(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
