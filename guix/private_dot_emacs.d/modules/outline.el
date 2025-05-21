(with-eval-after-load 'outshine
  (define-key outline-minor-mode-map (kbd "C-M-i") nil))

(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'scheme-mode-hook 'outline-minor-mode)
