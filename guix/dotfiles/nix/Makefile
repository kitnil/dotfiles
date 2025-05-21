HOSTNAME = $(shell hostname)

.PHONY: build
build:
	nix-shell --run "deploy --dry-activate --skip-checks --debug-logs .#$(HOSTNAME) -- --print-build-logs --show-trace"

.PHONY: install
install:
	nix-shell --run "deploy --skip-checks --debug-logs .#$(HOSTNAME) -- --print-build-logs --show-trace"
