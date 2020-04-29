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
        (".ansible-hosts" . yaml-mode)))
