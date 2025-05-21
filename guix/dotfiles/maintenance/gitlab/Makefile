all:
	TF_VAR_GITLAB_TOKEN=$(shell pass show gitlab.com-tokens/terraform) nix shell --command $(shell guix build --no-offload --load-path=../.. --file=main.scm)
