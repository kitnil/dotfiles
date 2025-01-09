container_registry=harbor.home.wugi.info
.ONESHELL:
vendir:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	tarball=$$(nix develop --command nix build .#container-vendir --show-trace --print-build-logs --no-link --print-out-paths)
	skopeo copy --insecure-policy docker-archive\:$$tarball docker://$(container_registry)/library/$@:$$commit_8
