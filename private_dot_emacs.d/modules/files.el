(mapc (lambda (lst)
        (add-to-list 'auto-mode-alist lst))
      '(("PKGBUILD" . shell-script-mode)
        ("\\.conkerorrc" . js-mode)
        ("\\.guile" . scheme-mode)
        ("\\.mbsyncrc" . conf-mode)
        ("manifest\\.scm" . scheme-mode)
        ("\\.tfstate" . json-mode)
        ("bashrc" . sh-mode)
        ("kresd\\.conf" . lua-mode)
        ("\\.luadoc" . lua-mode)
        ("\\.drv" . guix-derivation-mode)
        (".ansible-hosts" . yaml-mode)
        ("\\.bats" . sh-mode)))

;; Encrypted Chezmoi files with names like encrypted_example
(add-to-list 'file-name-handler-alist
             `(,(rx (and "encrypted_" (+ alnum))) . epa-file-handler))
