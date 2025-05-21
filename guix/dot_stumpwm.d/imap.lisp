(defvar *imap-recent* 0)

(defcommand imap-update-recent-count () ()
  (setq *imap-recent*
        (apply '+
               (mapcar (lambda (mailbox)
                         (parse-integer
                          (second (split-string (car (filter (lambda (str)
                                                               (uiop/utility:string-suffix-p str "RECENT"))
                                                             (mapcar (lambda (str)
                                                                       (string-trim '(#\Newline) str))
                                                                     (split-string (run-shell-command (format nil "curl --request 'EXAMINE ~a' --user 'oleg:~a' imap://localhost" mailbox (password-store-show "localhost/imap/oleg")) t)
                                                                                   '(#\^M)))))
                                                '(#\space)))))
                       '("INBOX" "majordomo"))))
  (mode-line-update))

