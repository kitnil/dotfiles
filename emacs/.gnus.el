(setq user-mail-address	"go.wigust@gmail.com"
      user-full-name	"Oleg Pykhalov"

      gnus-select-method  '(nnmaildir "natsu"
				      (directory "/home/natsu/Maildir")
				      (nnir-search-engine notmuch)
				      (nnir-notmuch-remove-prefix "/home/natsu/Maildir/"))
      gnus-permanently-visible-groups ".*INBOX"

      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-check-new-newsgroups nil
      gnus-thread-hide-subtree t)
