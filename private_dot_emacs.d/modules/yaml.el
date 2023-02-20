(add-hook 'yaml-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
