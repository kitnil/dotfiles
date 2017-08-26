(setq gnus-select-method
      '(nnimap "clover"
	       (nnimap-address "localhost")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps"))

      gnus-permanently-visible-groups ".*INBOX"

      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-check-new-newsgroups nil
      gnus-thread-hide-subtree t)

(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))


;; (Info-goto-node "(gnus) Scoring On Other Headers")
;; I e s p To RET <your name> RET

(setq gnus-extra-headers '(To Cc Newsgroups Keywords List-Id)
      nnmail-extra-headers gnus-extra-headers)
