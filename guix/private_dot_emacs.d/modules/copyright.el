(defun wi-fullname-and-email ()
  (format "%s <%s>" user-full-name user-mail-address))

(define-skeleton copyright
  "Insert a copyright by $USER notice at cursor."
  "FULL_NAME <EMAIL>: "
  comment-start
  "; Copyright © " `(format-time-string "%Y") " "
  (or (wi-fullname-and-email) str)
  '(if (copyright-offset-too-large-p)
       (message "Copyright extends beyond `copyright-limit' and won't\
be updated automatically."))
  comment-end \n)

(setq copyright-names-regexp (wi-fullname-and-email))

;; (setq copyright-limit nil) ;scan whole buffer instead of the default 2000 characters

(add-hook 'before-save-hook 'copyright-update)

(setq quickurl-format-function
      (lambda (url) (format "<%s>" (quickurl-url-url url))))
