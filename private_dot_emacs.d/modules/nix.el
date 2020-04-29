(setq nix-repl-executable-args
      (list "repl"
            (expand-file-name "~/.nix-defexpr/channels/nixos-unstable")))

(defvar wi-nix--prettify-symbols-alist
  '(("versionAtLeastCut" . ?≥)
    ("versionOlderCut" . ?<)
    ("deleteTests" . ?Ø)
    ("++" . 10746)
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
