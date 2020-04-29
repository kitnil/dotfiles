(defun wi-expand-file-names (files)
  "Expand FILES."
  (mapcar (lambda (file) (expand-file-name file)) files))

;; See <https://lists.gnu.org/archive/html/emacs-devel/2017-12/msg00017.html>.
(defun wi-git-log (&optional repo commit)
  "Check REPO for COMMIT and if it exists, display its commit message.
Interactively, prompt for REPO, defaulting to emacs-master, and
for COMMIT, defaulting to the commit hash at point."
  (interactive "p")
  (let* ((git-dir (if repo
		      (read-directory-name
                       "Repo: " "/mnt/data/steve/git/"
                       nil t "emacs-master")
		    "/mnt/data/steve/git/emacs-master"))
	 (commit0
          (or commit
              (read-string "Commit: " nil nil (word-at-point))))
	 (default-directory git-dir)
	 (output-buffer (get-buffer-create "*git log*"))
	 (proc (progn
		 (with-current-buffer output-buffer (erase-buffer))
		 (call-process "git" nil output-buffer nil
			       "branch" "--contains" commit0))))
    (when proc
      (with-current-buffer output-buffer
	(goto-char (point-min))
	(unless (looking-at "[ *]")
	  (user-error "%s is not on branch %s" commit0
		      (file-name-base git-dir)))
	(insert "Branches:\n")
	(goto-char (point-max))
	(call-process "git" nil output-buffer nil "log" "-1" commit0)
	(pop-to-buffer output-buffer)))))

(defun string-to-symbols (str)
  "Convert a STR string (for example \"ℕ₃₂\") to a list of characters (8469 (Br . Bl) 8323 (Br . Bl) 8322) suited for command `prettify-symbols-mode'."
  (let ((chars (string-to-list str)))
    `(,(car chars)
      ,@(apply 'append (mapcar (lambda (char)
                                 (list '(Br . Bl) char))
                               (cdr chars))))))

(defun delete-current-buffer-file ()
  "Delete the current buffer and the file connected with it"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (yes-or-no-p "Are you sure this file should be removed? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun wi-buffer-major-mode (buffer)
  "Return `major-mode' of BUFFER."
  (cdr (assoc 'major-mode (buffer-local-variables buffer))))

(defun wi-buffers-similar-major-mode ()
  "Return buffer with similar `major-mode' as in current buffer."
  (-filter (lambda (buffer)
             (string-equal (wi-buffer-major-mode (current-buffer))
                           (wi-buffer-major-mode buffer)))
           (buffer-list)))




