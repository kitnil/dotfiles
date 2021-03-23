;; Per-directory local variables for GNU Emacs 23 and later.

((nil
  . ((projectile-project-test-cmd . "make check")
     (eval . (setq-local geiser-guile-load-path
                         (list (concat (projectile-project-root)
                                       "dotfiles/guixsd/modules")))))))
