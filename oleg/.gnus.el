;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(require 'mailcap)

(setq gnus-use-full-window nil)

(setq gnus-select-method '(nnimap "USER"
                                  (nnimap-address "localhost")
                                  (nnimap-server-port "imaps")))

(setq gnus-permanently-visible-groups ".*INBOX")
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq gnus-check-new-newsgroups nil) ; Call `gnus-find-new-newsgroups' manually


;; (Info-goto-node "(gnus) Scoring On Other Headers")
;; I e s p To RET <your name> RET

(setq gnus-extra-headers '(To Cc Keywords Gcc Newsgroups X-GM-LABELS List-Id))
(setq nnmail-extra-headers gnus-extra-headers)

(setq gnus-parameters '(("^INBOX$" (gnus-thread-sort-functions
                                    'gnus-thread-sort-by-score))))

(add-hook 'message-sent-hook #'gnus-score-followup-thread)

;; Code from: https://github.com/jwiegley/dot-emacs
(defun switch-to-gnus (&optional arg)
  "Switch to a Gnus related buffer.
    Candidates are buffers starting with
     *mail or *reply or *wide reply
     *Summary or
     *Group*
    Use a prefix argument to start Gnus if no candidate exists."
  (interactive "P")
  (let (candidate
        (alist '(("^\\*\\(mail\\|\\(wide \\)?reply\\)" t)
                 ("^\\*Group")
                 ("^\\*Summary")
                 ("^\\*Article" nil (lambda ()
                                      (buffer-live-p
                                       gnus-article-current-summary))))))
    (catch 'none-found
      (dolist (item alist)
        (let (last
              (regexp (nth 0 item))
              (optional (nth 1 item))
              (test (nth 2 item)))
          (dolist (buf (buffer-list))
            (when (and (string-match regexp (buffer-name buf))
                       (> (buffer-size buf) 0))
              (setq last buf)))
          (cond ((and last (or (not test) (funcall test)))
                 (setq candidate last))
                (optional
                 nil)
                (t
                 (throw 'none-found t))))))
    (cond (candidate
           (switch-to-buffer candidate))
          (arg
           (gnus))
          (t
           (error "No candidate found")))))

(defun gnus-summary-limit-to-score (score &optional below)
  "Limit to articles with score at or above SCORE if BELOW is nil,
below otherwise."
  (interactive (list (string-to-number
                      (read-string
                       (format "Limit to articles with score of at %s: "
                               (if current-prefix-arg "most" "least"))))))
  (let* ((data gnus-newsgroup-data)
         (compare (if (or below current-prefix-arg) #'<= #'>=))
         articles)
    (while data
      (when (funcall compare (gnus-summary-article-score
                              (gnus-data-number (car data)))
                     score)
	(push (gnus-data-number (car data)) articles))
      (setq data (cdr data)))
    (prog1
	(gnus-summary-limit articles)
      (gnus-summary-position-point))))

(setq gnus-visible-headers
      (concat "^From:"
              "\\|^Newsgroups:"
              "\\|^Subject:"
              "\\|^Date:"
              "\\|^Followup-To:"
              "\\|^Reply-To:"
              "\\|^Organization:"
              "\\|^Summary:"
              "\\|^Keywords:"
              "\\|^To:"
              "\\|^[BGF]?Cc:"
              "\\|^Posted-To:"
              "\\|^Mail-Copies-To:"
              "\\|^Mail-Followup-To:"
              "\\|^Apparently-To:"
              "\\|^User-Agent:"
              "\\|^Message-ID:"
              "\\|^Gnus-Warning:"
              "\\|^Resent-From:"))

;; Mimetypes configured in <~/.mailcap>.
;; See <https://www.emacswiki.org/emacs/MimeTypesWithGnus>.

;; Emacs ignores mailcap for PDF
;; See <https://lists.gnu.org/archive/html/info-gnus-english/2016-04/msg00001.html>.

(setcdr
 (assoc "application" mailcap-mime-data)
 (remove '("pdf"
           (viewer . doc-view-mode)
           (type . "application/pdf")
           (test eq window-system 'x))
         (cdr (assoc "application" mailcap-mime-data))))

(setcdr
 (assoc "application" mailcap-mime-data)
 (remove '("pdf"
           (viewer . pdf-view-mode)
           (type . "application/pdf")
           (test eq window-system 'x))
         (cdr (assoc "application" mailcap-mime-data))))

;; Origin <http://bbdb.sourceforge.net/bbdb.html#SEC2>.
(bbdb-initialize 'gnus 'message)

;; Origin <https://emacs.stackexchange.com/a/166>.
(bbdb-mua-auto-update-init 'message) ; use 'gnus for incoming messages too
(setq bbdb-mua-auto-update-p 'query) ;; or 'create to create without asking

(defcustom wi-gnus-subject-regexp-debbugs
  (rx-to-string `(and (* "[") "bug" "#" (group (+ alnum)) (* "]") (+ anything))
                t)
  "Regexp matching Debbugs bug in Gnus header.")

(defun wi-gnus-subject-match-debbugs ()
  "Return Debbugs bug number from a Gnus article header."
  (interactive)
  (let* ((header (with-current-buffer gnus-summary-buffer
                   (gnus-summary-article-header)))
         (subject (mail-header-subject header)))
    (string-match wi-gnus-subject-regexp-debbugs subject)
    (string-to-number (match-string 1 subject))))

(defun wi-gnus-browse-debbugs ()
  "Open current Gnus article in with `debbugs-gnu-bugs'."
  (interactive)
  (debbugs-gnu-bugs (wi-gnus-subject-match-debbugs)))

(with-eval-after-load 'gnus-group
  (let ((map gnus-group-mode-map))
    (define-key map (kbd "<f5>") 'mbsync)))

(with-eval-after-load 'gnus
  (let ((map gnus-summary-mode-map))
    (define-key map (kbd "<f5>") 'mbsync)
    (define-key map (kbd "<f6>") 'org-capture)))
