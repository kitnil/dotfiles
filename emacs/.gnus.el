(setq gnus-select-method '(nnimap "USER"
                                  (nnimap-address "localhost")
                                  (nnimap-server-port "imaps")))

(setq gnus-permanently-visible-groups ".*INBOX")
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq gnus-check-new-newsgroups nil)


;; (Info-goto-node "(gnus) Scoring On Other Headers")
;; I e s p To RET <your name> RET

(setq gnus-extra-headers '(List-Id To))
(setq nnmail-extra-headers gnus-extra-headers)

(setq gnus-parameters '(("^INBOX$" (gnus-thread-sort-functions
                                    'gnus-thread-sort-by-score))))

(add-hook 'message-sent-hook #'gnus-score-followup-thread)
(add-hook 'gnus-summary-mode-hook #'hl-line-mode)

;; Code from: https://github.com/jwiegley/dot-emacs
(defun switch-to-gnus (&optional arg)
  "Switch to a Gnus related buffer.
    Candidates are buffers starting with
     *mail or *reply or *wide reply
     *Summary or
     *Group*
    Use a prefix argument to start Gnus if no candidate exists."
  (interactive "P")
  (let (candidate
        (alist '(("^\\*\\(mail\\|\\(wide \\)?reply\\)" t)
                 ("^\\*Group")
                 ("^\\*Summary")
                 ("^\\*Article" nil (lambda ()
                                      (buffer-live-p
                                       gnus-article-current-summary))))))
    (catch 'none-found
      (dolist (item alist)
        (let (last
              (regexp (nth 0 item))
              (optional (nth 1 item))
              (test (nth 2 item)))
          (dolist (buf (buffer-list))
            (when (and (string-match regexp (buffer-name buf))
                       (> (buffer-size buf) 0))
              (setq last buf)))
          (cond ((and last (or (not test) (funcall test)))
                 (setq candidate last))
                (optional
                 nil)
                (t
                 (throw 'none-found t))))))
    (cond (candidate
           (switch-to-buffer candidate))
          (arg
           (gnus))
          (t
           (error "No candidate found")))))
