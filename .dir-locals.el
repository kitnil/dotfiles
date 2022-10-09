;; Per-directory local variables for GNU Emacs 23 and later.

((nil
  . ((projectile-project-configure-cmd . "./configure")
     (projectile-project-test-cmd . "make check")
     (projectile-project-install-cmd . "make install")
     (eval . (progn
               (when (boundp #'projectile-project-root)
                     (setenv "GUIX_PACKAGE_PATH"
                             (concat (projectile-project-root)
                                     "dotfiles/guixsd/modules")))
               (setq-local geiser-guile-load-path
                           (append (list (concat (getenv "HOME") "/src/cgit.duckdns.org/guix/guix-wigust/guix"))
                                   (list (concat (getenv "HOME") "/src/gitlab.com/nonguix/nonguix"))
                                   (if (boundp #'projectile-project-root)
                                       (list (concat (projectile-project-root)
                                                     "dotfiles/guixsd/modules"))
                                     '()))))))))
