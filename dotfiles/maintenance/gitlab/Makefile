all:
	TF_VAR_GITLAB_TOKEN=$(shell pass show gitlab.com-tokens/terraform) nix shell --command $(shell guix build -L ../.. -f main.scm)
