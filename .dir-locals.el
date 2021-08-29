;; Per-directory local variables for GNU Emacs 23 and later.

((nil
  . ((projectile-project-configure-cmd . "./configure")
     (projectile-project-test-cmd . "make check")
     (projectile-project-install-cmd . "make install")
     (eval . (setq-local geiser-guile-load-path
                         (list (concat (projectile-project-root)
                                       "dotfiles/guixsd/modules")
                               (concat (getenv "HOME") "/src/guix-wigust/guix")))))))
