container_registry=harbor.home.wugi.info
.ONESHELL:
nixos-systemd:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	tarball=$$(nix develop --command nix build .#nixosConfigurations.nixos-systemd.config.system.build.tarball --show-trace --print-build-logs --no-link --print-out-paths)/tarball/nixos-system-x86_64-linux.tar.xz
	image_id=$$(docker import $$tarball)
	docker tag $$image_id "$(container_registry)/library/$@:$$commit_8"
	docker push "$(container_registry)/library/$@:$$commit_8"
	echo "$(container_registry)/library/$@:$$commit_8"

dotfiles-home-manager:
	nix flake lock --update-input dotfiles-home-manager
	git commit -m 'flake.lock: Update dotfiles-home-manager input.' flake.lock
