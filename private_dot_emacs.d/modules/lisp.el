
;;;
;;; Elisp
;;;

(defvar wi-elisp--prettify-symbols-alist
  '(("lambda" . ?λ)
    ("lambda*" . (?λ (Br . Bl) ?*)))
  "Alist of symbol prettifications for `emacs-lisp-mode'.")

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'prettify-symbols-alist)
                 wi-elisp--prettify-symbols-alist)))

(add-hook 'emacs-lisp-mode-hook #'(lambda () (setq fill-column 70)))


;;;
;;; Slime
;;;

(setq slime-net-coding-system 'utf-8-unix)
(setq slime-truncate-lines nil)


;;;
;;; hy
;;;

(defconst wi-hy--prettify-symbols-alist
  '(("fn" . ?λ)
    ("True" . (?# (Br . Bl) ?t))
    ("False" . (?# (Br . Bl) ?f))))

(add-hooks
 '(((hy-mode-hook)
    . (lambda ()
        (set (make-local-variable 'prettify-symbols-alist)
             wi-hy--prettify-symbols-alist)))))
