(defcustom debbugs-gnus-bug-number-regexp-group 1
  "Regexp group in `debbugs-gnus' for prettifying."
  :type 'integer
  :group 'debbugs-gnus)

(defcustom debbugs-gnus-bug-number-regexp
  (rx "[" "bug" "#" (group (* (any "0-9" "a-z"))) "]")
  "Regexp matching Debbugs bug number"
  :type 'regexp
  :group 'debbugs-gnu)

(defcustom debbugs-gnu-favorite
  nil
  "List of favorite bugs."
  :type 'list
  :group 'debbugs-gnu)

(defun debbugs-gnus-bug-number ()
  "Return bug number of current article."
  (let ((subject (mail-header-subject gnus-current-headers)))
    (string-match debbugs-gnus-bug-number-regexp subject)
    (string-to-number (match-string debbugs-gnus-bug-number-regexp-group subject))))

(defun debbugs-gnu-add-favorite ()
  "Add bug to favorite."
  (interactive)
  (add-to-list 'debbugs-gnu-favorite (debbugs-gnus-bug-number)))

(defun debbugs-gnu-list-favorite ()
  "List favorite bugs."
  (interactive)
  (eval (cons 'debbugs-gnu-bugs debbugs-gnu-favorite)))


