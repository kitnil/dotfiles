(add-hook 'yaml-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

(add-hook 'yaml-mode-hook 'yaml-pro-mode)


(defun wi-yaml-prettify-symbols ()
  (set (make-local-variable 'prettify-symbols-alist)
       `(("xn--c1ajpcpgs.xn--90a1af.xn--p1ai" . ,(string-to-symbols "хостинг.спб.рф"))
         ("xn--e1arhcfp.xn--90a1af.xn--p1ai" . ,(string-to-symbols "хостер.спб.рф")))))

(add-hook 'yaml-mode-hook 'wi-yaml-prettify-symbols)
