(defconst wi-c--prettify-symbols-alist
  `((" % " . (? (Br . Bl) ?m
                (Br . Bl) ?o
                (Br . Bl) ?d
                (Br . Bl) ? ))
    (" * " . (? (Br . Bl) ?·
                (Br . Bl) ? ))
    (" / " . (? (Br . Bl) ?÷
                (Br . Bl) ? ))
    ("!" . ?¬)
    ("!=" . ?≢)
    ("&&" . ?∧)
    ("->" . (?  (Br . Bl) ?→
                (Br . Bl) ? ))
    ("<=" . ?≤)
    ("==" . ?≡)
    (">=" . ?≥)
    ("NULL" . ?N)
    ("false" . ?F)
    ("float" . ?ℚ)
    ("int" . ?ℤ)
    ("rand" . ?𝔼)
    ("true" . ?T)
    ("uint32_t" . (?ℕ (Br . Bl) ?₃
                      (Br . Bl) ?₂))
    ("uint8_t" . (?ℕ (Br . Bl) ?₈))
    ("Uint32" . ,(string-to-symbols "ℕ₃₂"))
    ("Uint8" . ,(string-to-symbols "ℕ₈"))
    ("union" . ?∪)
    ("void" . ?Ø)
    ("x_1" . (?x (Br . Bl) ?₁))
    ("x_2" . (?x (Br . Bl) ?₂))
    ("y_1" . (?y (Br . Bl) ?₁))
    ("y_2" . (?y (Br . Bl) ?₂))
    ("||" . ?∨)))

(add-hook 'c-mode-hook 'prettify-symbols-mode)
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'prettify-symbols-alist)
                 wi-c--prettify-symbols-alist)))

(with-eval-after-load 'c-mode
  (let ((map c-mode-map))
    (define-key map (kbd "M-.") 'dumb-jump-go)))

(with-eval-after-load 'cc-vars
  (setq-default c-cleanup-list '(space-before-funcall scope-operator)))

(with-eval-after-load 'semantic
  (global-semantic-decoration-mode t))

(defconst wi-asm-mode--prettify-symbols-alist
  `(("%eax" . ,(string-to-symbols "%eax (ℕ₃₂)"))
    ("%edi" . ,(string-to-symbols "%edi (ℕ₃₂)"))
    ("%rbp" . ,(string-to-symbols "%rbp (base→)"))
    ("%rsp" . ,(string-to-symbols "%rsp (stack→)"))
    ("$0x0" . (?0))))

(add-hook 'asm-mode-hook 'prettify-symbols-mode)
(add-hook 'asm-mode-hook
          (lambda ()
            (set (make-local-variable 'prettify-symbols-alist)
                 wi-asm-mode--prettify-symbols-alist)))

(setq disaster-cc "gcc")
