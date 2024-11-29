container_registry=harbor.home.wugi.info
.ONESHELL:
tor-bridges:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	tarball=$$(nix build .# --show-trace --print-build-logs --no-link --print-out-paths)
	skopeo copy docker-archive:$$tarball "docker://$(container_registry)/library/$@:$$commit_8"
	echo "$(container_registry)/library/$@:$$commit_8"
