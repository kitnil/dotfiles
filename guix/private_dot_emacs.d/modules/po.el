(add-hook 'po-subedit-mode-hook (lambda () (ispell-change-dictionary "ru")))
(add-hook 'po-subedit-mode-hook '(lambda () (activate-input-method "russian-computer")))
(add-hook 'po-subedit-exit-hook (lambda () (ispell-buffer)))
(add-hook 'po-mode-hook 'git-auto-commit-mode)
