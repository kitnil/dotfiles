(add-hook 'yaml-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

(add-hook 'yaml-mode-hook 'yaml-pro-mode)
