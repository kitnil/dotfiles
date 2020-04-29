(setq ffap-file-finder 'org-open-file)

(defcustom ffap-info-finder 'info
  "The command called by `wi-info-at-point' to find an Info file."
  :type 'function
  :group 'ffap
  :risky t)

(defun wi-info-at-point (&optional filename)
  "Start Info, defaulting to file at point.  See `ffap'.

Optional argument FILENAME opens the file instead."
  (interactive)
  (or filename (setq filename (thing-at-point 'filename t)))
  (cond
   ((and ffap-info-regexp
         (string-match ffap-info-regexp filename))
    (funcall ffap-info-finder filename))
   ((error "No such file or directory `%s'" filename))))

(defun delete-file-at-point (&optional filename)
  "Remove file, defaulting to file at point.

Optional argument FILENAME removes the file instead."
  (interactive)
  (or filename (setq filename (thing-at-point 'filename t)))
  (delete-file filename))

(autoload 'browse-at-remote--remote-ref "browse-at-remote")

(defun wi-github-issue-at-point (&optional issue)
  "Start `browse-url', defaulting to ISSUE at point.  See `ffap'."
  (interactive)
  (or issue (setq issue (thing-at-point 'number t)))
  (if (numberp issue)
      (browse-url
       (concat (car (browse-at-remote--remote-ref default-directory))
               "/issues/" (number-to-string issue)))
    (error "No issue number at point `%s'" issue)))

(autoload 'fci-mode "fill-column-indicator"
  "Indicate the location of the fill column by drawing a thin
line at fill column." t)

(defun ffap-info-p (filename)
  "If FILENAME is Info page, return it."
  (when (string-match-p (rx-to-string `(and ".info"
                                            (zero-or-more ".gz")
                                            line-end)
                                      t)
                        filename)
    filename))

(defun ffap-man-p (filename)
  "If FILENAME if Man page, return it."
  (when (string-match-p (rx "/man" (zero-or-more digit)
                            "/" (one-or-more (or alphanumeric "." "-" "_"))
                            (zero-or-more ".gz")
                            line-end)
                        filename)
    filename))

(autoload 'guix-ffap-store-path-p "guix-ffap")

(defcustom guix-profile-path-regexp
  (rx-to-string `(and line-start
                      (or "~" ,(getenv "HOME")) "/.guix-profile/"))
  "Regexp matching Guix profile path."
  :type 'regexp
  :group 'guix)

(defun guix-ffap-profile-path-p (filename)
  "Match FILENAME with `guix-profile-path-regexp' regexp and return it."
  (when (string-match-p guix-profile-path-regexp filename) filename))

(defun wi-find-file-at-point (&optional filename)
  "Find FILENAME, guessing a default from text around point.
If `ffap-url-regexp' is not nil, the FILENAME may also be an URL.
With a prefix, this command behaves exactly like `ffap-file-finder'.
If `ffap-require-prefix' is set, the prefix meaning is reversed.
See also the variables `ffap-dired-wildcards', `ffap-newfile-prompt',
and the functions `ffap-file-at-point' and `ffap-url-at-point'."
  (interactive)
  (if (and (called-interactively-p 'interactive)
	   (if ffap-require-prefix (not current-prefix-arg)
	     current-prefix-arg))
      ;; Do exactly the ffap-file-finder command, even the prompting:
      (let (current-prefix-arg)		; we already interpreted it
	(call-interactively ffap-file-finder))
    (or filename (setq filename (ffap-prompter)))
    (let ((url (ffap-url-p filename))
          (info-page (ffap-info-p filename))
          ;; (guix-profile-dir (guix-ffap-profile-path-p filename))
          (man-page (ffap-man-p filename)))
      (cond
       (url
	(let (current-prefix-arg)
	  (funcall ffap-url-fetcher url)))
       (info-page
        (let (current-prefix-arg)
          (info info-page)))
       (man-page
        (let (current-prefix-arg)
          (man man-page)))
       ;; (guix-profile-dir
       ;;  (let (current-prefix-arg)
       ;;    (guix-run-in-shell (concat "readlink " filename))))
       ((and ffap-pass-wildcards-to-dired
	     ffap-dired-wildcards
	     (string-match ffap-dired-wildcards filename))
	(funcall ffap-directory-finder filename))
       ((and ffap-dired-wildcards
	     (string-match ffap-dired-wildcards filename)
	     find-file-wildcards
	     ;; Check if it's find-file that supports wildcards arg
	     (memq ffap-file-finder '(find-file find-alternate-file)))
	(funcall ffap-file-finder (expand-file-name filename) t))
       ((or (not ffap-newfile-prompt)
	    (file-exists-p filename)
	    (y-or-n-p "File does not exist, create buffer? "))
	(funcall ffap-file-finder
		 ;; expand-file-name fixes "~/~/.emacs" bug sent by CHUCKR.
		 (expand-file-name filename)))
       ;; User does not want to find a non-existent file:
       ((signal 'file-error (list "Opening file buffer"
				  "No such file or directory"
				  filename)))))))

(with-eval-after-load 'ffap
  (add-to-list 'ffap-alist '("\\.patch" . guix-devel-ffap-patch)))

(advice-add 'find-file-at-point :override #'wi-find-file-at-point)
