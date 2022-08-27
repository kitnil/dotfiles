all:
	TF_VAR_GITLAB_TOKEN=$(shell pass show bq/gitlab01.bqtstuff.com/tokens/personal) nix shell --command $(shell guix build -L ../.. -f main.scm)
