
;;;
;;; Company
;;;

;; Popup completion framework
(with-eval-after-load 'company
  (setq company-clang-insert-arguments nil)
  (setq company-gtags-insert-arguments nil)
  (setq company-semantic-insert-arguments nil)
  (add-to-list 'company-backends 'company-abbrev)
  (add-to-list 'company-backends 'company-lsp)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))


;;;
;;; Ivy
;;;

(ivy-rich-mode 1)
(setq ivy-height 30)
(setq ivy-format-function #'ivy-format-function-line)
