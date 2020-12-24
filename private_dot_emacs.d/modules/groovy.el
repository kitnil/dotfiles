(define-auto-insert
  "Jenkinsfile"
  ["groovy/Jenkinsfile" yas-expand-current-buffer])

(add-hook 'groovy-mode-hook 'fci-mode)

;; (with-eval-after-load 'groovy-mode
;;   (let ((map groovy-mode-map))
;;     (define-key map (kbd "M-.") 'dumb-jump-go)))

(add-hook 'jenkinsfile-mode-hook
          #'(lambda () (setq-local page-delimiter "^\s*stage\(")))
