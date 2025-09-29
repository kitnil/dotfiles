(add-hook 'yaml-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

(when (functionp #'yaml-pro-mode)
  (add-hook 'yaml-mode-hook 'yaml-pro-mode))


(defun wi-yaml-prettify-symbols ()
  (set (make-local-variable 'prettify-symbols-alist)
       `(("xn--c1ajpcpgs.xn--90a1af.xn--p1ai" . ,(string-to-symbols "хостинг.спб.рф"))
         ("xn--e1arhcfp.xn--90a1af.xn--p1ai" . ,(string-to-symbols "хостер.спб.рф")))))

(add-hook 'yaml-mode-hook 'wi-yaml-prettify-symbols)
(add-hook 'yaml-mode-hook
          '(lambda ()
             (set (make-local-variable 'yas-indent-line) 'nil)))

(defun yaml-code-block-edit ()
  "Edit the current synopsis/description in `texinfo-mode'."
  (interactive)
  (let* ((begin (region-beginning))
         (end (region-end))
         (edit-indirect-guess-mode-function
          (lambda (&rest _)
            (scheme-mode))))
    (edit-indirect-region begin end 'display-buffer)))


;;;
;;; edit-indirect
;;;

;; https://github.com/Fanael/edit-indirect/issues/6
(require 's)
(require 'dash)

(defvar edit-indirect--left-margin 0)

(defun vbe/compute-left-margin (code)
  "Compute left margin of a string of code."
  (-min
   (-map #'(lambda (line) (length (car (s-match "^\\s-*" line))))
         (-remove 's-blank? (s-lines code)))))

(defun vbe/edit-indirect/remove-left-margin ()
  "Remove left-margin and save it into a local variable."
  (let ((lm (vbe/compute-left-margin (buffer-substring (point-min) (point-max)))))
    (indent-rigidly (point-min) (point-max) (* -1 lm))
    (setq-local edit-indirect--left-margin lm)
    ;; https://github.com/Fanael/edit-indirect/issues/6#issuecomment-1055542145
    ;; buffer-local variable whose value should not be reset when changing major modes
    (put 'edit-indirect--left-margin 'permanent-local t)))

(defun vbe/edit-indirect/restore-left-margin ()
  "Restore left-margin before commiting."
  (indent-rigidly (point-min) (point-max) edit-indirect--left-margin))

(add-hook 'edit-indirect-after-creation-hook #'vbe/edit-indirect/remove-left-margin)
(add-hook 'edit-indirect-before-commit-hook #'vbe/edit-indirect/restore-left-margin)

(require 'edit-indirect)
;; https://github.com/Fanael/edit-indirect/issues/6#issuecomment-1284144173
(define-key edit-indirect-mode-map [remap save-buffer] #'edit-indirect-commit)
