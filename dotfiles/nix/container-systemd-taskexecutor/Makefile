container_registry=harbor.home.wugi.info
.ONESHELL:
nixos-systemd-taskexecutor:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	container=$$(nix develop --command nix build .#nixosConfigurations.nixos-systemd.config.system.build.tarball --show-trace --print-build-logs --no-link --print-out-paths)/tarball/nixos-system-x86_64-linux.tar.xz
	skopeo copy docker-archive\:$$container docker://$(container_registry)/library/$@:$$commit_8
	echo "$(container_registry)/library/$@:$$commit_8"

original:
	nix flake lock --update-input original
	git commit -m 'flake.lock: Update original input.' flake.lock
