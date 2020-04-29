(cl-defun wi-debbugs-gnu-list (&optional (mail-address user-mail-address)
                                         (not-suppress nil))
  "List bugs on debbugs.gnu.org from USER-MAIL-ADDRESS.

With NOT-SUPPRESS non-nil argument include archived bugs."
  (interactive)
  (let ((debbugs-gnu-current-query `((submitter . ,mail-address))))
    (if (or current-prefix-arg not-suppress)
        (debbugs-gnu nil nil nil nil)
        (debbugs-gnu nil nil nil t))))

(defun wi-debbugs-get-url (bug-number)
  "Get a debbugs url according to `BUG-NUMBER'"
  (interactive "sBug number: ")
  (kill-new (concat "https://debbugs.gnu.org/cgi/bugreport.cgi?bug="
                    bug-number)))

(defun wi-debbugs-gnu-apply-patch (&optional branch)
  "Apply the patch from the current message.
If given a prefix, patch in the branch directory instead."
  (interactive "P")
  (add-hook 'emacs-lisp-mode-hook 'debbugs-gnu-lisp-mode)
  (add-hook 'diff-mode-hook 'debbugs-gnu-diff-mode)
  (add-hook 'change-log-mode-hook 'debbugs-gnu-change-mode)
  (debbugs-gnu-init-current-directory branch)
  (let ((rej (expand-file-name "debbugs-gnu.rej" temporary-file-directory))
	(output-buffer (get-buffer-create "*debbugs patch*"))
	(patch-buffers nil))
    (when (file-exists-p rej)
      (delete-file rej))
    (with-current-buffer output-buffer
      (erase-buffer))
    (gnus-summary-select-article nil t)
    ;; The patches are either in MIME attachements or the main article
    ;; buffer.  Determine which.
    (with-current-buffer gnus-article-buffer
      (dolist (handle (mapcar 'cdr (gnus-article-mime-handles)))
	(when
	    (string-match "diff\\|patch\\|plain" (mm-handle-media-type handle))
	  (push (cons (mm-handle-encoding handle)
		      (mm-handle-buffer handle))
		patch-buffers))))
    (unless patch-buffers
      (gnus-summary-show-article 'raw)
      (article-decode-charset)
      (push (cons nil gnus-article-buffer) patch-buffers))
    (dolist (elem patch-buffers)
      (with-current-buffer (generate-new-buffer "*debbugs input patch*")
	(insert-buffer-substring (cdr elem))
	(cond ((eq (car elem) 'base64)
	       (base64-decode-region (point-min) (point-max)))
	      ((eq (car elem) 'quoted-printable)
	       (quoted-printable-decode-region (point-min) (point-max))))
	(debbugs-gnu-fix-patch debbugs-gnu-current-directory)
	(call-process-region (point-min) (point-max)
			     "patch" nil output-buffer nil
			     "-r" rej "--no-backup-if-mismatch"
			     "-l" "-f"
			     "-d" (expand-file-name
				   debbugs-gnu-current-directory)
			     "-p1")))
    (set-buffer output-buffer)
    (when (file-exists-p rej)
      (goto-char (point-max))
      (insert-file-contents-literally rej))
    (goto-char (point-max))
    (save-some-buffers t)
    (wi-compile-guix (expand-file-name debbugs-gnu-current-directory))
    (vc-dir debbugs-gnu-current-directory)
    (vc-dir-hide-up-to-date)
    (goto-char (point-min))
    (sit-for 1)
    (vc-diff)
    ;; All these commands are asynchronous, so just wait a bit.  This
    ;; should be done properly a different way.
    (sit-for 2)
    ;; We've now done everything, so arrange the windows we need to see.
    (delete-other-windows)
    (switch-to-buffer output-buffer)
    (split-window)
    (split-window)
    (other-window 1)
    (switch-to-buffer "*compilation*")
    (goto-char (point-max))
    (other-window 1)
    (switch-to-buffer "*vc-diff*")
    (goto-char (point-min))))

;; Set defaults for debbugs-gnu commands
(with-eval-after-load 'debbugs-gnu
  (setq debbugs-gnu-default-packages (list "guix" "guix-patches")))
