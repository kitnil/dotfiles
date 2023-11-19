(put 'dired-find-alternate-file 'disabled nil)

(setq dired-listing-switches (purecopy "-alh")) ; Prettify dired
(setq dired-hide-details-hide-symlink-targets nil)

(defun wi-file-name-directory ()
  "Open dired which contains current file."
  (interactive)
  (dired (file-name-directory (file-truename (buffer-file-name)))))

(with-eval-after-load 'dired
  (require 'dired-x)
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-omit-files
        (concat dired-omit-files  "\\|.*\\.~$"))
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c x") 'crux-open-with)
    (define-key map (kbd "<f8>") 'crux-open-with)
    (define-key map (kbd "<home>") 'beginning-of-buffer)
    (define-key map (kbd "<end>") 'end-of-buffer)
    (define-key map (kbd "C-j") 'dired-find-file)
    (define-key map (kbd "<mouse-2>") 'crux-open-with))

  (add-hook 'dired-mode-hook 'dired-omit-mode))
