(add-hook 'java-mode-hook 'highlight-indent-guides-mode)

(defconst wi-java--prettify-symbols-alist
  `(("@Bean" . ,(string-to-symbols "@instance-of-a-class")) ;fancy word
    ("@Autowired" . ,(string-to-symbols "@field-injection"))
    ("$$" . ,(string-to-symbols "enhanced")) ;you could this in debug
    ))

(when (functionp #'add-hooks)
  (add-hooks
   '(((java-mode-hook)
      . (lambda ()
          (set (make-local-variable 'prettify-symbols-alist)
               wi-java--prettify-symbols-alist))))))

;; Copied and modified from:
;; https://github.com/emacs-lsp/lsp-java/issues/26
;;
;; Java Configuration
(let ((directory
       ;; TODO: Do not hardcode, e.g. use `lsp-java-server-install-dir'.
       (concat (file-name-directory "/home/oleg/.emacs.d/.cache/lsp/eclipse.jdt.ls/")
               "lombok")))

  (setq lombok-library-path (concat (file-name-directory directory)
                                    "lombok.jar"))

  (unless (file-exists-p lombok-library-path)
    (url-copy-file "https://projectlombok.org/downloads/lombok.jar"
                   lombok-library-path))

  ;; TODO: do not hardcode, e.g. use `lsp-java-vmargs'.
  (setq lsp-java-vmargs
        (append (list (concat "-javaagent:"
                              (expand-file-name lombok-library-path)))
                '("-noverify"
		  "-Xmx1G"
		  "-XX:+UseG1GC"
		  "-XX:+UseStringDeduplication"))))
