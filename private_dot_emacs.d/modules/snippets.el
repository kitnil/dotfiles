;; Inspired by https://github.com/suzp1984/donkey/blob/master/elisp/auto-insert/my-auto-insert.el

(defun yas-expand-current-buffer ()
  "Expand all yasnippet snippets in a current buffer."
  (interactive)
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(with-eval-after-load 'yasnippet
  (setq yas-snippet-dirs
        (append (wi-expand-file-names
                 '("~/.emacs.d/snippets"
                   "~/src/guix/etc/snippets"
                   "~/.guix-profile/share/emacs/yasnippet-snippets"))
                yas-snippet-dirs))
  (yas-reload-all))

(add-hook 'find-file-hook 'auto-insert)
