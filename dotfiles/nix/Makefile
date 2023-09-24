.PHONY: build
build:
	nix-shell --run 'deploy --dry-activate . -- -L'

.PHONY: install
install:
	nix-shell --run 'deploy . -- -L'
