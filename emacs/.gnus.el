(setq gnus-select-method
      '(nnimap "clover"
	       (nnimap-address "localhost")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps"))

      gnus-permanently-visible-groups ".*INBOX"

      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-check-new-newsgroups nil
      gnus-thread-hide-subtree t)

(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))
