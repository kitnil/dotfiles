(setq user-mail-address	"go.wigust@gmail.com"
      user-full-name	"Oleg Pykhalov"

      gnus-select-method
      '(nnimap "clover"
	       (nnimap-address "localhost")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps"))

      gnus-permanently-visible-groups ".*INBOX"

      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-check-new-newsgroups nil
      gnus-thread-hide-subtree t)
