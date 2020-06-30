(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
(setq lsp-ui-sideline-enable nil) ;right menu
(setq lsp-file-watch-threshold nil)
(setq lsp-lua-emmy-jar-path (expand-file-name "EmmyLua-LS-all.jar" user-emacs-directory))

;; GNU Emacs 26.3 (build 1, x86_64-pc-linux-gnu, GTK+ Version 3.24.20)
;; https://github.com/emacs-lsp/lsp-java/issues/173
(defun seq-first (sequence)
  "Return the first element of SEQUENCE."
  (seq-elt sequence 0))
