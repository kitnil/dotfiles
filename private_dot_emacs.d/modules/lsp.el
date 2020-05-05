(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
(setq lsp-ui-sideline-enable nil) ;right menu
(setq lsp-file-watch-threshold nil)
(setq lsp-lua-emmy-jar-path (expand-file-name "EmmyLua-LS-all.jar" user-emacs-directory))
