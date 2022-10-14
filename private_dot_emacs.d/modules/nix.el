(setq nix-repl-executable-args
      (list "repl"
            (expand-file-name "~/.nix-defexpr/channels/nixos-unstable")))

(defvar wi-nix--prettify-symbols-alist
  '(("versionAtLeastCut" . ?≥)
    ("versionOlderCut" . ?<)
    ("deleteTests" . ?Ø)
    ("++" . ⧺)
    ("==" . ?≡)
    ("!=" . ?≠)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("generic" . ?g)
    ("version" . ?v)
    ("optional" . ?o)
    ("optionals" . (?o (Br . Bl) ?s)))
  "Alist of symbol prettifications for `nix-mode'.")

(add-hook 'nix-mode-hook
          (lambda ()
            (set (make-local-variable 'prettify-symbols-alist)
                 wi-nix--prettify-symbols-alist)))

(defun dumb-jump-init-nixpkgs ()
  (interactive)
  (with-temp-file ".dumbjump" (insert "+/home/oleg/src/nixpkgs")))

(add-hook 'nix-mode-hook 'goto-address-mode)

(define-auto-insert
  "shell.nix"
  ["nix/shell.nix" yas-expand-current-buffer])

(define-auto-insert
  "flake.nix"
  ["nix/flake.nix" yas-expand-current-buffer])
