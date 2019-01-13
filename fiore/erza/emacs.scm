(use-modules (erza emacs))

(emacs-configuration
 (hooks
  (list (emacs-hook (hook 'prog-mode-hook)
                    (procedures '(hs-minor-mode
                                  rainbow-delimiters-mode
                                  yas-minor-mode)))))
 (extra-options '()))
