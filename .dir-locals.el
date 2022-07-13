;; Per-directory local variables for GNU Emacs 23 and later.

((nil
  . ((projectile-project-configure-cmd . "./configure")
     (projectile-project-test-cmd . "make check")
     (projectile-project-install-cmd . "make install")
     (eval . (setq-local geiser-guile-load-path
                         (append (list (concat (getenv "HOME") "/src/guix-wigust/guix"))
                                 (if (boundp #'projectile-project-root)
                                     (list (concat (projectile-project-root)
                                                   "dotfiles/guixsd/modules"))
                                   '())))))))
