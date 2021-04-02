(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

;; (require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'kotlin-mode-hook 'lsp)
(add-hook 'go-mode-hook 'lsp)
(add-hook 'python-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(setq lsp-ui-sideline-enable nil) ;right menu
(setq lsp-file-watch-threshold nil)
(setq lsp-lua-emmy-jar-path (expand-file-name "EmmyLua-LS-all.jar" user-emacs-directory))

;; TODO: Spring
;; (require 'lsp-java-boot)
;; ;; to enable the lenses
;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

;; GNU Emacs 26.3 (build 1, x86_64-pc-linux-gnu, GTK+ Version 3.24.20)
;; https://github.com/emacs-lsp/lsp-java/issues/173
(defun seq-first (sequence)
  "Return the first element of SEQUENCE."
  (seq-elt sequence 0))


;;;
;;; Python
;;;

(setq lsp-pyls-plugins-flake8-max-line-length 110)
(setq lsp-pyls-plugins-pycodestyle-max-line-length 110)
