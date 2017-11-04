(setq gnus-select-method
      '(nnimap "USER"
               ;; It could also be imap.googlemail.com if that's your server.
               (nnimap-address "localhost")
               (nnimap-server-port "imaps"))

      gnus-permanently-visible-groups ".*INBOX"

      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-check-new-newsgroups nil
      gnus-thread-hide-subtree t)


;; (Info-goto-node "(gnus) Scoring On Other Headers")
;; I e s p To RET <your name> RET

(setq gnus-extra-headers '(List-Id To)
      nnmail-extra-headers gnus-extra-headers)

(setq gnus-parameters
      '(("^INBOX$"
         (gnus-thread-sort-functions 'gnus-thread-sort-by-score))))

(mailcap-add-mailcap-entry "image"
                           "jpeg"
                           '((viewer  . "feh %s")
                             (type    . "image/jpeg")))

(mailcap-add-mailcap-entry "image"
                           "jpg"
                           '((viewer  . "feh %s")
                             (type    . "image/jpg")))

(add-hook 'message-sent-hook #'gnus-score-followup-thread)
(add-hook 'gnus-summary-mode-hook #'hl-line-mode)
