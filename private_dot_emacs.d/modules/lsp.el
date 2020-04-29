(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
(setq lsp-ui-sideline-enable nil) ;right menu
(setq lsp-file-watch-threshold nil)
(setq lsp-lua-emmy-jar-path (expand-file-name "EmmyLua-LS-all.jar" user-emacs-directory))
