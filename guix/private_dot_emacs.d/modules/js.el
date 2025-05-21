(setq wi-js--prettify-symbols-alist
      `(("!" . ?¬)
        ("!!" . ,(string-to-symbols "Boolean $ ")) ;Haskell like notation
        ("!=" . ?≠)
        ("!==" . ?≠)
        ("===" . ?≡)
        ("&&" . ?∧)
        ("||" . ?∨)
        ("<=" . ?≤)
        (">=" . ?≥)
        (";" . ,(string-to-symbols " "))
        ("null" . ?N)
        ("false" . ?F)
        ("true" . ?T)
        ("=>" . ?→)
        ("const" . ,(string-to-symbols "constant"))
        ("instanceof" . ,(string-to-symbols "instance of"))))

(add-hook 'js-mode-hook 'prettify-symbols-mode)
(add-hook 'js-mode-hook
          (lambda ()
            (set (make-local-variable 'prettify-symbols-alist)
                 wi-js--prettify-symbols-alist)))
