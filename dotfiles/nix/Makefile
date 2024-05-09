HOSTNAME = $(shell hostname)

.PHONY: build
build:
	nix-shell --run "deploy --dry-activate .#$(HOSTNAME) -- -L"

.PHONY: install
install:
	nix-shell --run "deploy .#$(HOSTNAME) -- -L"
