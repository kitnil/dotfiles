(setq mml-secure-insert-signature 'always)
(setq mail-user-agent 'gnus-user-agent)

;; (setq epg-config--program-alist
;;       '((OpenPGP epg-gpg-program
;;                  ;; ("gpg2" . "2.1.6")
;;                  ("gpg" . "1.4.3"))
;;         (CMS epg-gpgsm-program ("gpgsm" . "2.0.4"))))

(with-eval-after-load 'mailcap
  (add-to-list 'mailcap-mime-extensions '(".scm" . "text/x-scheme")))

(defun wi-send-buffer-as-mail ()
  "Send current buffer as body in email."
  (interactive)
  (let ((str (buffer-string)))
    (compose-mail)
    (save-excursion
      (message-goto-body)
      (insert str))))

;; Simple Mail Transfer Protocol (SMTP)
(with-eval-after-load 'sendmail
  (setq send-mail-function #'smtpmail-send-it)
  (setq smtpmail-smtp-server "smtp.gmail.com"))

(setq smtpmail-queue-mail t) ; Call after typing M-x `smtpmail-send-queued-mail'


;;;
;;; Notmuch
;;;

(autoload 'notmuch-search "notmuch" nil t)

(with-eval-after-load 'notmuch ;overwrites by default
  (setq mail-user-agent 'gnus-user-agent))

;; XXX: Make async before uncommenting
;; (run-with-timer 0 (* 20 60) '(lambda ()
;;                                (interactive)
;;                                (unless (get-buffer "*Summary INBOX*")
;;                                  (notmuch-poll))))


;;;
;;; StumpWM
;;;

(defun stumpwm-imap-update-recent-count ()
  "Call imap-update-recent-count in StumpWM."
  (interactive)
  (call-process "sh" nil nil nil "-c" "echo '(imap-update-recent-count)' | stumpish -e eval"))
