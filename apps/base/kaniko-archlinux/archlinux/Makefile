container_registry=harbor.home.wugi.info
.ONESHELL:
archlinux-systemd:
	set -o nounset -o errexit -o pipefail -o xtrace
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	docker build --network=host --pull --tag "$(container_registry)/library/$@:$$commit_8" .
	docker push "$(container_registry)/library/$@:$$commit_8"
	echo "docker://$(container_registry)/library/$@:$$commit_8"
