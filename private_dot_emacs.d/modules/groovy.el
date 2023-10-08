(define-auto-insert
  "Jenkinsfile"
  ["groovy/Jenkinsfile" yas-expand-current-buffer])

(add-hook 'groovy-mode-hook 'display-fill-column-indicator-mode)

;; (with-eval-after-load 'groovy-mode
;;   (let ((map groovy-mode-map))
;;     (define-key map (kbd "M-.") 'dumb-jump-go)))

(add-hook 'jenkinsfile-mode-hook
          #'(lambda () (setq-local page-delimiter "^\s*stage\(")))


;; XXX: Automatically build and install groovy-language-server-all.jar
;; ~/src/github.com/GroovyLanguageServer/groovy-language-server$ cp build/libs/groovy-language-server-all.jar /home/oleg/.emacs.d/.cache/lsp/groovy-language-server-all.jar
