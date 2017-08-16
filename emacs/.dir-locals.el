;; Per-directory local variables for GNU Emacs 23 and later.

((nil
  . ((fill-column . 78)
     (tab-width   .  8)
     (sentence-end-double-space . t)

     (eval . (add-hook 'compilation-mode-hook 'guix-build-log-minor-mode))
     (eval . (add-hook 'compilation-mode-hook
                       (lambda () (setq-local truncate-lines 1))))
     (eval . (add-hook 'shell-mode-hook 'guix-build-log-minor-mode))))
 (c-mode          . ((c-file-style . "gnu")))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)))
 (texinfo-mode    . ((indent-tabs-mode . nil)
                     (fill-column . 72))))
