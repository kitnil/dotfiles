(add-hook 'java-mode-hook 'highlight-indent-guides-mode)

(defconst wi-java--prettify-symbols-alist
  `(("@Bean" . ,(string-to-symbols "@instance-of-a-class")) ;fancy word
    ("@Autowired" . ,(string-to-symbols "@field-injection"))
    ("$$" . ,(string-to-symbols "enhanced")) ;you could this in debug
    ))

(add-hooks
 '(((java-mode-hook)
    . (lambda ()
        (set (make-local-variable 'prettify-symbols-alist)
             wi-java--prettify-symbols-alist)))))
