(add-hooks
 '(((diff-mode-hook dired-mode-hook proced-post-display-hook
     shell-mode-hook ibuffer-mode-hook guix-env-var-mode-hook)
    . guix-prettify-mode)
   ((dired-mode-hook) . toggle-truncate-lines)
   ((scheme-mode-hook) . guix-devel-mode)
   ((guix-devel-mode magit-revision-mode-hook) . goto-address-mode)
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
