;; Origin <https://changelog.complete.org/archives/9865-emacs-2-introducing-org-mode>.
(setq org-ellipsis "…")

(setq plain-org-wiki-directory (expand-file-name "~/src/org/"))

(setq org-descriptive-links nil)

;; (with-eval-after-load 'org
;;   (setq org-format-latex-options
;;         (plist-put org-format-latex-options :scale 1.5))
;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))

(with-eval-after-load 'org
  (require 'org-protocol) ; For `org-capture' from Xorg

  (add-to-list 'org-file-apps '("\\.png\\'" . system))

  (org-babel-do-load-languages
      'org-babel-load-languages
      '((R . t)
        (emacs-lisp . t)
        (scheme . t)
        (shell . t))))

(setq org-startup-folded 'showall) ; Show all in `org-mode' at startup

(setq org-email-link-description-format "Email %c: %s") ; More than 30 character

(setq org-capture-templates
      '(("c" "Note" item (file "~/.notes") "%?")

        ("f" "File email" entry (file+headline "inbox.org" "Email")
         "* %U %a by [[mailto:%:fromaddress][%:fromname]]"
         :immediate-finish nil
         :prepend nil)

        ;; Requires org-capture-extension
        ;; https://github.com/sprig/org-capture-extension
        ("l" "Protocol" item (file "web.org")
         "[[%:link][%:description]]\n%i"
         :immediate-finish t)
        ("L" "Protocol Link" item (file "web.org")
         "[[%:link][%:description]]"
         ;; (concat "* TODO [[%:link][%:description]]\nSCHEDULED: " (format-time-string "<%Y-%m-%d %a>"))
         :immediate-finish t)

        ("X" "emacs-org-capture" item (file "web.org")
         "[[%:link]]"
         ;; (concat "* TODO [[%:link]]\nSCHEDULED: " (format-time-string "<%Y-%m-%d %a>"))
         :immediate-finish t)

        ("r" "Respond ro email" entry (file+headline "inbox.org" "Email")
         "[[mailto:%:fromaddress][%:fromname]]"
         :immediate-finish t
         :prepend t)

        ("t" "Tasks" entry (file+headline ".notes" "Tasks")
         "* TODO %? \n%T" :prepend t)

        ("b" "buffer" entry (file "TODO.org") "* TODO [[%F]]%?")

        ("B" "blog" plain (file "blog.org") "%?")
        ("e" "emacs" plain (file "emacs.org") "%?")
        ("g" "guix" plain (file "guix.org") "%?")
        ("i" "Templates for TODO")
        ("ii" "TODO" entry (file "TODO.org") "* TODO %?")
        ("ir" "TODO region" entry (file "TODO.org") "* TODO %i"
         :immediate-finish t)
        ("m" "music" plain (file "music.org") "%?")
        ("n" "pdfview" item (file "pdf.org") "%a %?")
        ("o" "misc" plain (file "misc.org") "%?")
        ("p" "phrase" item (file "phrase.org") "%?")
        ("r" "read" plain (file "read.org") "%?")
        ("v" "video" plain (file "video.org") "%?")
        ("V" "watch" plain (file "watch.org") "%?")
        ("w" "work" plain (file "work.org") "%?")))

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/src/org/"
         :base-extension "org"
         :publishing-directory "/var/www/org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 ; Just the default for this project.
         :auto-preamble t
         :auto-sitemap t
         :sitemap-filename "index.org")))

(defun wigust-mir-org-uniq ()
  "Remove duplicate subheadings, preserving order."
  ;; See <http://lists.gnu.org/archive/html/emacs-orgmode/2018-01/msg00000.html>.
  (interactive)
  (let ((seen (make-hash-table :test 'equal))
        (removed 0))
    (save-excursion
      (org-map-entries (lambda ()
                         (let ((heading (org-get-heading t t t t)))
                           (if (not (gethash heading seen))
                               (puthash heading t seen)
                             (org-cut-subtree)
                             (org-backward-heading-same-level 1)
                             (setq removed (1+ removed)))))
                       (format "LEVEL=%s" (1+ (org-current-level)))
                       'tree))
    (message "Removed %d duplicates" removed)))

;; See <http://mbork.pl/2017-12-04_Embedding_files_in_Org-mode>.
(defun wi-org-insert-file (filename)
  "Insert Elisp code block recreating file named FILENAME."
  (interactive "f")
  (let ((base64-string
	 (with-temp-buffer
	   (insert-file-contents-literally filename)
	   (base64-encode-region (point-min) (point-max))
	   (buffer-string))))
	(insert (format "#+BEGIN_SRC emacs-lisp :results output silent\n  (with-temp-file %S\n    (insert (base64-decode-string\n      %S)))\n#+END_SRC" filename base64-string))))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;;;
;;; Redmine
;;;

(require 'org-redmine)
(setq org-redmine-config-default-limit 100)
(setq org-redmine-limit 100)

