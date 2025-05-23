container_registry=harbor.home.wugi.info
.ONESHELL:
nixos-systemd-taskexecutor:
	set -o nounset -o errexit -o pipefail -o xtrace
	IMG=$(container_registry)/library/$@:$$(git rev-parse --abbrev-ref HEAD)-$$(git rev-parse HEAD | cut -c -8)-$$(date +%s)
	tarball=$$(nix build .#nixosConfigurations.nixos-systemd.config.system.build.tarball --show-trace --print-build-logs --no-link --print-out-paths)/tarball/nixos-system-x86_64-linux.tar.xz
	image_id=$$(docker import $$tarball)
	docker tag $$image_id $$IMG
	docker push $$IMG
	echo $$IMG

original:
	nix flake lock --update-input original
	git commit -m 'flake.lock: Update original input.' flake.lock
