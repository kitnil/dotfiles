(put 'dired-find-alternate-file 'disabled nil)

(setq dired-listing-switches (purecopy "-alh")) ; Prettify dired
(setq dired-hide-details-hide-symlink-targets nil)

(defun wi-file-name-directory ()
  "Open dired which contains current file."
  (interactive)
  (dired (file-name-directory (file-truename (buffer-file-name)))))

(with-eval-after-load 'dired
  (require 'dired-x)
  (setq dired-guess-shell-alist-default
        (cons (list "\\.pdf\\'" "zathura")
              (delete (assoc "\\.pdf\\'" dired-guess-shell-alist-default)
                      dired-guess-shell-alist-default)))
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c x") 'crux-open-with)
    (define-key map (kbd "<f8>") 'crux-open-with)
    (define-key map (kbd "<home>") 'beginning-of-buffer)
    (define-key map (kbd "<end>") 'end-of-buffer)
    (define-key map (kbd "C-j") 'dired-find-file)))
