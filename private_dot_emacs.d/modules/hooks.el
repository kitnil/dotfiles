(when (functionp #'add-hooks)
  (add-hooks
   '(((dired-mode-hook) . toggle-truncate-lines)
     ((guix-devel-mode magit-revision-mode-hook) . goto-address-mode)
     ((geiser-repl-mode-hook shell-mode-hook)
      . (lambda ()
          (setq bidi-display-reordering nil)))

     ;; XXX: Breaks C-z in scheme-mode (run-geiser)
     ;; ((geiser-repl-mode-hook shell-mode-hook)
     ;;  . (lambda ()
     ;;      (font-lock-mode -1)))

     ((erc-mode-hook) . (lambda () (setq truncate-lines t))))))

(when (functionp #'add-hooks)
  (when (functionp 'guix-prettify-mode)
    (add-hooks
     '(((diff-mode-hook dired-mode-hook proced-post-display-hook
                        shell-mode-hook ibuffer-mode-hook guix-env-var-mode-hook)
        . guix-prettify-mode))))
  (when (functionp 'yas-minor-mode)
    (add-hooks '(((prog-mode-hook org-mode-hook text-mode-hook) . yas-minor-mode))))
  (when (boundp 'rainbow-delimiters-mode)
    (add-hooks '(((prog-mode-hook geiser-repl-mode-hook) . rainbow-delimiters-mode))))
  (when (functionp 'smartparens-strict-mode)
    (add-hooks '(((prog-mode-hook
                   minibuffer-inactive-mode-hook
                   geiser-repl-mode-hook
                   git-commit-mode-hook
                   org-mode-hook)
                  . smartparens-strict-mode))))
  (when (functionp 'hs-minor-mode)
    (add-hooks '(((prog-mode-hook) . hs-minor-mode))))
  (when (functionp 'guix-devel-mode)
    (add-hooks '(((scheme-mode-hook) . guix-devel-mode)))))
