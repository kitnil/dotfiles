(dashboard-setup-startup-hook)
(setq dashboard-items '((agenda . 10)
                        (registers . 10)
                        (recents . 15)
                        (projects . 30)
                        (bookmarks . 20)))
(setq show-week-agenda-p t)
;; (setq dashboard-org-agenda-categories '("Tasks")) ; version above 1.6.0 required
;; (with-eval-after-load 'dashboard
;;   (let ((map dashboard-mode-map))
;;     (define-key map (kbd "n") 'dashboard-next-section)
;;     (define-key map (kbd "p") 'dashboard-previous-section)))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-page-separator "\n\n")
(setq initial-scratch-message nil) ; Don't put text in *scratch* buffer
