(use-modules (gnu)
             (guix profiles))

(use-package-modules admin cmake databases emacs emacs-xyz mail ocaml package-management)

(use-modules (wigust packages emacs))

;; emacs-guix is not compatible with geiser newer than 0.10
;; (module-set! (resolve-module '(gnu packages emacs-xyz)) 'emacs-geiser emacs-geiser-0.10)

(module-set! (resolve-module '(gnu packages emacs-xyz)) 'emacs-slime emacs-slime-2.24)

(packages->manifest
 (list emacs-next-pgtk
       emacs-academic-phrases
       emacs-ace-window
       emacs-add-hooks
       emacs-ag
       emacs-aggressive-indent  ; Auto indent minor mode
       emacs-ansible-doc
       emacs-auto-yasnippet
       emacs-bbdb
       emacs-beginend
       emacs-browse-at-remote
       emacs-build-farm
       emacs-cmake-mode
       emacs-company            ; Complition framework
       emacs-company-quickhelp  ; Help pages for Company
       emacs-constants
       emacs-helm-projectile
       emacs-counsel-tramp
       emacs-crux
       emacs-debbugs ; <https://debbugs.gnu.org/> interface
       emacs-debpaste           ; Front end to <https://paste.debian.net/>
       emacs-default-text-scale ; Scale text in all buffers
       emacs-dhall-mode
       emacs-dumb-jump
       emacs-ebuild-mode
       emacs-edit-indirect
       emacs-edit-server ; See <https://github.com/stsquad/emacs_chrome/>.
       emacs-elfeed             ; RSS reader
       emacs-elfeed-score
       emacs-emamux
       ;; emacs-emms
       emacs-emojify
       emacs-erc-hl-nicks       ; for ERC
       emacs-epkg
       emacs-eshell-bookmark
       ;; emacs-esup
       emacs-ewmctrl            ; Control X windows from Emacs
       emacs-execline
       emacs-expand-region
       emacs-deadgrep           ; ripgrep front-end
       emacs-f3
       emacs-fd
       emacs-fancy-narrow
       emacs-ffap-rfc-space
       emacs-fill-column-indicator
       emacs-flycheck                 ; Syntax checker
       emacs-jenkinsfile-mode
       emacs-jsonnet-mode
       emacs-geiser                   ; Scheme bridge
       emacs-ggtags             ; Front end to GNU Global
       emacs-git-auto-commit-mode
       emacs-gif-screencast
       emacs-git-gutter
       emacs-git-timemachine
       emacs-gitlab-ci-mode
       emacs-gitpatch           ; Send patches
       emacs-go-mode
       emacs-god-mode           ; Commands without modifier keys
       emacs-grep-context
       emacs-groovy-modes
       emacs-guix
       emacs-helm               ; Narrowing framework
       emacs-helm-c-yasnippet
       emacs-helm-eww
       emacs-helm-firefox       ; Search for bookmarks in Icecat
       emacs-helm-gtags
       emacs-helm-make          ; Front end to `make'
       emacs-helm-mode-manager
       emacs-helm-shell-history
       emacs-hideshowvis
       emacs-highlight-defined
       emacs-highlight-sexp
       emacs-highlight-stages   ; Highlight code stages
       emacs-hl-todo
       emacs-htmlize
       emacs-hy-mode
       emacs-hydra
       emacs-ibuffer-projectile
       emacs-irfc
       emacs-ivy                ; Complition framework
       emacs-ivy-posframe
       emacs-ivy-rich
       emacs-ivy-yasnippet
       emacs-know-your-http-well
       emacs-kotlin-mode
       emacs-lice-el
       emacs-lsp-java
       emacs-lsp-mode
       emacs-lsp-ui
       emacs-lua-mode
       emacs-macrostep
       emacs-magit
       emacs-magit-org-todos-el
       emacs-magit-todos
       emacs-forge
       emacs-markdown-mode      ; Commonmark major mode
       emacs-matcha
       emacs-mbsync
       emacs-move-text
       emacs-multifiles
       emacs-multiple-cursors   ; Multi cursor
       emacs-multi-vterm
       emacs-nginx-mode
       emacs-nix-mode           ; Nix language mode
       emacs-notmuch
       emacs-olivetti
       emacs-org-appear
       emacs-org-bullets
       ;; TODO: Fix build emacs-org-generate
       emacs-org-mind-map       ; General mind maps from Org files
       emacs-orgit
       emacs-org-roam
       emacs-company-org-block
       emacs-outshine           ; Emacs outline-mode
       emacs-package-lint
       emacs-pcre2el
       emacs-phi-search
       emacs-phi-search-mc
       emacs-php-mode
       emacs-polymode
       emacs-polymode-ansible
       emacs-projectile         ; Project functions
       emacs-rainbow-delimiters ; Prettify parentheses
       emacs-rainbow-mode       ; Show colors in codes
       emacs-refactor
       emacs-rust-mode
       emacs-org-redmine
       emacs-recutils
       emacs-restclient
       emacs-rotate-text
       emacs-scheme-complete
       emacs-scratch-el
       emacs-slack
       ;; XXX: Fix build emacs-slime              ; Sbcl repl
       emacs-ssh-config-mode
       emacs-smart-mode-line
       emacs-smartparens        ; Structured editing
       emacs-string-inflection
       emacs-suggest
       emacs-symbol-overlay
       emacs-terraform-mode
       emacs-transmission       ; Front end to transmission-daemon
       emacs-transpose-frame    ; M-x transpose-frame
       emacs-tuareg             ; ocaml
       emacs-undo-tree          ; Undo visualisation
       emacs-use-package        ; Lazy configuration
       emacs-validate
       emacs-modus-themes
       emacs-w3m                ; Front end to w3m command line web browser
       emacs-which-key          ; Key bindings help
       emacs-yaml-mode          ; YAML files
       emacs-yasnippet          ; Snippets
       emacs-yasnippet-snippets ; Collection of snippets
       emacs-deadgrep
       emacs-vterm
       emacs-vterm-toggle
       emacs-wgrep
       emacs-znc

       epipe

       emacs-docker
       emacs-docker-tramp
       emacs-dockerfile-mode
       emacs-docker-compose-mode

       emacs-ssh-config-mode

       emacs-xterm-color

       ;; XXX: Failed to build emacs-benchmark-init
       ))
