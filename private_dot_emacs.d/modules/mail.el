;; only sign
(add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

;; https://lists.gnu.org/archive/html/guix-devel/2020-09/msg00024.html
(setq mml-secure-openpgp-sign-with-sender t)

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
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-stream-type 'starttls)
  (setq smtpmail-debug-info t)
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
;;; Guix
;;;

(defun guix-patch-reply ()
  (interactive)
  (message-goto-body)
  ;; GPG
  (when (string= "<#secure method=pgpmime mode=sign>"
                 (buffer-substring (line-beginning-position) (line-end-position)))
    (beginning-of-line)
    (next-logical-line))
  ;; Body
  (insert "Hi,\n\n")
  (search-forward-regexp "diff --git")
  (search-forward-regexp "@@ ")
  (beginning-of-line)
  (next-logical-line)
  (newline 1)
  (open-line 2)
  (insert "[…]")
  (delete-region (point) (point-max))
  (newline 2)
  (insert "Pushed to master.\n\nThanks,\nOleg.")
  ;; CC
  (message-goto-cc)
  (search-backward-regexp "@")
  (insert "-done"))
