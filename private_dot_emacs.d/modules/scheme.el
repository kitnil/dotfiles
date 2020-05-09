(defun projectile-run-guile (&optional pure)
  "Invoke ‘run-guile’ in the project’s root.

Interactively a non-nil prefix argument cleans `geiser-guile-load-path'.

Non-interactively, this uses the optional second argument PURE."
  (interactive)
  (let* ((geiser-guile-load-path (if (or pure current-prefix-arg) nil
                                   geiser-guile-load-path))
         (geiser-guile-load-path
          (append (list (directory-file-name
                         (expand-file-name
                          (projectile-project-root))))
                  geiser-guile-load-path)))
    (run-guile)))

(defun run-guile-with-directory (directory)
  "Invoke `run-guile' and add DIRECTORY to %`load-path'."
  (interactive "DAdd to %%load-path: ")
  (let ((geiser-guile-load-path (append (list directory)
                                        geiser-guile-load-path)))
    (run-guile)))

(defun emacs-add-to-load-path (directory)
  "Add DIRECTORY to `load-path'."
  (interactive "DAdd to `load-path': ")
  (add-to-list 'load-path (expand-file-name directory)))

(with-eval-after-load 'geiser
  (setq geiser-active-implementations '(guile))
  (setq geiser-default-implementation 'guile))

(with-eval-after-load 'geiser-guile
  ;; (add-to-list 'geiser-guile-load-path (expand-file-name "~/src/guix"))
  (setq geiser-guile-binary '("guile" "--no-auto-compile"))

  ;; Origin <https://gnunet.org/bot/log/guile/2018-02-24>
  ;; (setq geiser-guile-load-path (f-entries "~/src"))
  )

(defconst wi-scheme--prettify-symbols-alist
  '(("lambda" . ?λ)
    ("lambda*" . (?λ (Br . Bl) ?*))))

(add-hooks
 '(((scheme-mode-hook geiser-repl-mode-hook)
    . (lambda ()
        (set (make-local-variable 'prettify-symbols-alist)
             wi-scheme--prettify-symbols-alist)))))

(with-eval-after-load 'guix-repl
  (setq guix-directory (expand-file-name "~/src/guix-master")))

(autoload 'scheme-smart-complete "scheme-complete" nil t)
;; (eval-after-load 'scheme
;;    '(define-key scheme-mode-map "\e\t" 'scheme-smart-complete))
(setq scheme-default-implementation 'guile)
(setq *scheme-use-r7rs* nil)


;;;
;;; Hide
;;;

(defcustom wi-scheme-mode-toggle-hs-minor-mode nil
  "If non-nil enable `hs-minor-mode' in `scheme-mode'."
  :type 'boolean)

(defun wi-scheme-mode-toggle-hs-minor-mode ()
  (interactive)
  (if wi-scheme-mode-toggle-hs-minor-mode
      (progn (remove-hook 'scheme-mode-hook 'hs-minor-mode)
             (setq wi-scheme-mode-toggle-hs-minor-mode nil))
    (progn (add-hook 'scheme-mode-hook 'hs-minor-mode)
           (add-hook 'scheme-mode-hook #'hs-hide-all)
           (setq wi-scheme-mode-toggle-hs-minor-mode t))))


;;;
;;; Snippets
;;;

(define-auto-insert
  (rx "guile" (one-or-more (or alphanumeric "-")) line-end)
  ["guile/script" scheme-mode yas-expand-current-buffer])


