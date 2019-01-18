(use-modules (gnu)
             (guix profiles))

(use-package-modules cmake emacs emacs-xyz package-management)

(packages->manifest
 (list emacs-academic-phrases
       emacs-ace-window
       emacs-add-hooks
       emacs-ag
       emacs-aggressive-indent  ; Auto indent minor mode
       emacs-auto-yasnippet
       emacs-bash-completion
       emacs-bbdb
       emacs-beginend
       emacs-browse-at-remote
       emacs-build-farm
       emacs-cmake-mode
       emacs-company            ; Complition framework
       emacs-company-quickhelp  ; Help pages for Company
       emacs-constants
       emacs-crux
       emacs-debbugs ; <https://debbugs.gnu.org/> interface
       emacs-default-encrypt    ; Sign mail automatically
       emacs-default-text-scale ; Scale text in all buffers
       emacs-dumb-jump
       emacs-edit-indirect
       emacs-edit-server ; See <https://github.com/stsquad/emacs_chrome/>.
       emacs-elfeed             ; RSS reader
       emacs-emms
       emacs-erc-hl-nicks       ; for ERC
       emacs-esup
       emacs-ewmctrl            ; Control X windows from Emacs
       emacs-expand-region
       emacs-f3
       emacs-fancy-narrow
       emacs-ffap-rfc-space
       emacs-fill-column-indicator
       emacs-flycheck                 ; Syntax checker
       emacs-geiser                   ; Scheme bridge
       emacs-ggtags             ; Front end to GNU Global
       emacs-git-auto-commit-mode
       emacs-git-gutter
       emacs-git-timemachine
       emacs-gitpatch           ; Send patches
       emacs-go-mode
       emacs-god-mode           ; Commands without modifier keys
       emacs-grep-context
       emacs-guix
       emacs-helm               ; Narrowing framework
       emacs-helm-eww
       emacs-helm-firefox       ; Search for bookmarks in Icecat
       emacs-helm-gtags
       emacs-helm-make          ; Front end to `make'
       emacs-helm-mode-manager
       emacs-helm-projectile    ; Helm interface for Projectile
       emacs-helm-shell-history
       emacs-highlight-sexp
       emacs-highlight-stages   ; Highlight code stages
       emacs-highlight-symbol
       emacs-hl-todo
       emacs-htmlize
       emacs-hy-mode
       emacs-hydra
       emacs-ibuffer-projectile
       emacs-irfc
       emacs-ivy                ; Complition framework
       emacs-know-your-http-well
       emacs-lice-el
       emacs-lua-mode
       emacs-macrostep
       emacs-magit
       emacs-magit-org-todos-el
       emacs-markdown-mode      ; Commonmark major mode
       emacs-mbsync
       emacs-move-text
       emacs-multiple-cursors   ; Multi cursor
       emacs-nginx-mode
       emacs-nix-mode           ; Nix language mode
       emacs-org                ; Org
       emacs-org-bullets
       emacs-org-mind-map       ; General mind maps from Org files
       emacs-outshine           ; Emacs outline-mode
       emacs-php-mode
       emacs-projectile         ; Project functions
       emacs-rainbow-delimiters ; Prettify parentheses
       emacs-rainbow-mode       ; Show colors in codes
       emacs-restclient
       emacs-scheme-complete
       emacs-slime              ; Sbcl repl
       emacs-smart-mode-line
       emacs-smartparens        ; Structured editing
       emacs-string-inflection
       emacs-suggest
       emacs-transmission       ; Front end to transmission-daemon
       emacs-transpose-frame    ; M-x transpose-frame
       emacs-undo-tree          ; Undo visualisation
       emacs-use-package        ; Lazy configuration
       emacs-validate
       emacs-w3m                ; Front end to w3m command line web browser
       emacs-which-key          ; Key bindings help
       emacs-yaml-mode          ; YAML files
       emacs-yasnippet          ; Snippets
       emacs-yasnippet-snippets ; Collection of snippets
       emacs-znc

       epipe))
