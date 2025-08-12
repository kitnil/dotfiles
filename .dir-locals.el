;; Per-directory local variables for GNU Emacs 23 and later.

((nil
  . ((projectile-project-configure-cmd . "./configure")
     (projectile-project-test-cmd . "make check")
     (projectile-project-install-cmd . "make install")
     (eval . (progn
               (setq-local geiser-repl-add-project-paths nil)
               (when (functionp #'aggressive-indent-mode)
		 (aggressive-indent-mode)))))))
